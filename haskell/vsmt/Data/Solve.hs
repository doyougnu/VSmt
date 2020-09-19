-----------------------------------------------------------------------------
-- |
-- Module    : Data.Solve
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module that solves a variational smt problem
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

module Data.Solve where

import           Control.Monad.Except       (MonadError)
import Control.Monad.Logger
  ( LoggingT
  , MonadLogger(..)
  -- , NoLoggingT(runNoLoggingT)
  , logDebug
  , runStdoutLoggingT
  )
import qualified Control.Monad.State.Strict as St (MonadState, StateT, get,
                                                   gets, modify', runStateT,put)
import           Control.Monad.Trans        (MonadIO, MonadTrans, lift)
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Text                  as Text
import           Prelude                    hiding (EQ, GT, LT, log)

import qualified Data.SBV                   as S
import           Data.SBV.Internals         (SolverContext (..))
import qualified Data.SBV.Trans             as T
import qualified Data.SBV.Trans.Control     as C

import           Data.Core.Result
import           Data.Core.Types
import           Data.Core.Pretty

import Debug.Trace

------------------------------ Template Haskell --------------------------------
log :: MonadLogger m => Text.Text -> m ()
log = $(logDebug)

-- TODO custom loggers for each function, i.e., [DEBUG:EVAL]: ...
logWith :: (MonadLogger m, Show a) => Text.Text -> a -> m ()
logWith msg value = log $ msg <> sep <> Text.pack (show value)
  where sep :: Text.Text
        sep = " : "

logPretty :: (Pretty a, MonadLogger m, Show a) => Text.Text -> a -> m ()
logPretty msg value = log $ msg <> sep <> pretty value
  where sep :: Text.Text
        sep = " : "

logState :: Solver ()
logState = St.get >>= logWith "State: "

------------------------------ Internal Api -------------------------------------
findVCore :: IL -> Solver VarCore
findVCore = evaluate

solution :: (St.MonadState State m, Has Result) => m Result
solution = extract <$> St.get

-- | TODO abstract over the logging function
-- | TODO pending on server create, create a load function to handle injection
-- to the IL type
solveVerbose :: Proposition -> Maybe VariantContext -> IO (Result, State)
solveVerbose  i vConf =
    -- (context,dims) <- runStdoutLoggingT $ St.runStateT (contextToSBool conf) mempty{config=conf}
    T.runSMTWith T.z3{T.verbose=True} $
      do let _ = fromMaybe true vConf
         (il, st) <- runStdoutLoggingT $ runSolver mempty $ toILSymbolic i
         C.query $ runStdoutLoggingT $ runSolver st $
           findVCore il >>= removeChoices >> solution
           -- do core <- findVCore il
           --    _ <- choose core
           --    solution

solveForCoreVerbose :: Proposition -> Maybe VariantContext -> IO (VarCore, State)
solveForCoreVerbose  i vConf =
    -- (context,dims) <- runStdoutLoggingT $ St.runStateT (contextToSBool conf) mempty{config=conf}
    T.runSMTWith T.z3{T.verbose=True} $
      do let _ = fromMaybe true vConf
         (il, st) <- runStdoutLoggingT $ runSolver mempty $ toILSymbolic i
         C.query $ runStdoutLoggingT $ runSolver st $
           do core <- findVCore il
              logWith "Proposition: " i
              logWith "Core: " core
              logWith "Is Core Unit: " (isUnit core)
              return core

satVerbose :: Proposition -> Maybe VariantContext -> IO Result
satVerbose = (fmap fst .) <$> solveVerbose

------------------------------ Data Types --------------------------------------
-- | Solver configuration is a mapping of dimensions to boolean values, we
-- express this in two ways, first we hold a store of dimensions to symbolic
-- booleans to track what we have seen, secondly we hold a variant context _as_
-- a symbolic formula rather than a data structure so that we can spin up a
-- separate thread to check variant context sat calls when removing choices
type Store = Map.HashMap

type Ints         = Store Var T.SInteger
type Doubles      = Store Var T.SDouble
type Bools        = Store Var T.SBool
type Dimensions   = Store Dim T.SBool
type Context      = Store Dim Bool

class IxStorable ix where
  type Container ix :: * -> *
  type Container ix = Map.HashMap ix -- the default to hashmap

  add    :: ix -> elem -> Container ix elem -> Container ix elem
  isIn   :: ix -> Container ix elem -> Bool
  find   :: ix -> Container ix elem -> Maybe elem
  adjust :: (elem -> elem) -> ix -> Container ix elem -> Container ix elem

  -- | an unsafe version of find'
  -- TODO: encode the totality of find in the type system
  find'  :: ix -> Container ix elem -> elem
  find' = (fromJust .) . find

instance IxStorable Text.Text where add    = Map.insert
                                    isIn   = Map.member
                                    find   = Map.lookup
                                    adjust = Map.adjust

instance IxStorable Dim where add    = Map.insert
                              isIn   = Map.member
                              find   = Map.lookup
                              adjust = Map.adjust


-- | That which contains that has that
class Has that where
  type Contains that
  type Contains that = State
  extract :: Contains that -> that
  wrap    :: that -> Contains that -> Contains that

  -- this `by` that
  by :: Contains that -> (that -> that) -> Contains that
  by this f = flip wrap this . f . extract $ this

-- avoiding lens and generic-deriving dependencies
instance Has SVariantContext where
  type Contains SVariantContext = State
  extract     = sConfig
  wrap    c w = w{sConfig = c}

instance Has Ints    where extract   = ints
                           wrap    i w = w{ints = i}

instance Has Doubles where extract   = doubles
                           wrap    d w = w{doubles = d}

instance Has Bools   where extract   = bools
                           wrap    b w = w{bools = b}

instance Has Result  where extract   = result
                           wrap    r w = w{result=r}

instance Has Dimensions where extract   = dimensions
                              wrap    d w = w{dimensions=d}

instance Has Context where extract     = config
                           wrap    d w = w{config=d}

instance Has (Maybe VariantContext) where extract     = vConfig
                                          wrap    d w = w{vConfig=d}


type SVariantContext = S.SBool
instance Semigroup SVariantContext where (<>) = (&&&)
instance Monoid    SVariantContext where mempty = true


-- | The internal state of the solver is just a record that accumulates results
-- and a configuration to track choice decisions. We make a trade off of memory
-- for speed and represent the configuration in several ways. We keep a setline
-- representation of the config to support setlike operations most notably
-- `member`, we keep a formula representation to send to the result module and
-- we keep the symbolic representation to send to the solver. If we were missing
-- any of these then we would need to translate one to the other which will cost
-- constant time _for every_ choice, hence we want to make that constant factor
-- as small as possible
data State = State
    { result     :: Result
    , vConfig    :: Maybe VariantContext  -- the formula representation of the config
    , config     :: Context         -- a map or set representation of the config
    , sConfig    :: SVariantContext -- the symbolic representation of the config
    , ints       :: Ints
    , doubles    :: Doubles
    , bools      :: Bools
    , dimensions :: Dimensions
    } deriving Show

instance Semigroup State where
  a <> b = State { result     = result  a <> result  b
                 , config     = config  a <> config  b
                 , vConfig    = vConfig a <> vConfig b
                 , sConfig    = sConfig a <> sConfig b
                 , ints       = ints    a <> ints    b
                 , doubles    = doubles a <> doubles b
                 , bools      = bools   a <> bools   b
                 , dimensions = dimensions a <> dimensions b
                 }

instance Monoid State where
  mempty = State{ result     = mempty
                , config     = mempty
                , sConfig    = mempty
                , vConfig    = mempty
                , ints       = mempty
                , doubles    = mempty
                , bools      = mempty
                , dimensions = mempty
                }

-- TODO remove the StateT dependency for ReaderT
-- | A solver is just a reader over a solver enabled monad. The reader
-- maintains information during the variational execution, such as
-- configuration, variable stores
newtype SolverT m a = SolverT { runSolverT :: St.StateT State m a }
  deriving ( Functor,Applicative,Monad,MonadIO -- base
           , MonadTrans, MonadError e, MonadLogger
           , St.MonadState State, T.MonadSymbolic, C.MonadQuery
           )

-- | Unfortunately we have to write this one by hand. This type class tightly
-- couples use to SBV and is the mechanism to constrain things in the solver
instance (Monad m, SolverContext m) => SolverContext (SolverT m) where
  constrain       = lift . T.constrain
  softConstrain   = lift . T.softConstrain
  setOption       = lift . S.setOption
  namedConstraint = (lift .) . T.namedConstraint
  addAxiom        = (lift .) . T.addAxiom
  contextState    = lift contextState
  constrainWithAttribute = (lift .) . T.constrainWithAttribute

instance (Monad m, SolverContext m) => SolverContext (LoggingT m) where
  constrain       = lift . T.constrain
  softConstrain   = lift . T.softConstrain
  setOption       = lift . S.setOption
  namedConstraint = (lift .) . T.namedConstraint
  addAxiom        = (lift .) . T.addAxiom
  contextState    = lift contextState
  constrainWithAttribute = (lift .) . T.constrainWithAttribute

instance C.MonadQuery m    => C.MonadQuery (LoggingT m)    where queryState  = lift C.queryState
instance T.MonadSymbolic m => T.MonadSymbolic (LoggingT m) where symbolicEnv = lift T.symbolicEnv

-- | A solver type enabled with query operations and logging
type Solver = SolverT (LoggingT C.Query)
type PreSolver = SolverT (LoggingT T.Symbolic)

runSolver :: State -> SolverT m a -> m (a, State)
runSolver s = flip St.runStateT s . runSolverT

class Show a => Constrainable m a b where cached :: a -> m b

-- TODO fix this duplication
instance Constrainable Solver Var IL where
  cached ref = do
    st <- St.get
    case find ref $ extract st of
      Just x -> return (Ref x)
      Nothing -> do
        newSym <- T.label (Text.unpack ref) <$> C.freshVar (Text.unpack ref)
        St.modify' (`by` add ref newSym)
        return (Ref newSym)

instance Constrainable Solver Dim T.SBool where
  cached ref = do
    st <- St.get
    case find ref $ extract st of
      Just x -> return x
      Nothing -> do
        newSym <- T.label ref <$> C.freshVar ref
        St.modify' (`by` add ref newSym)
        return newSym

instance Constrainable Solver (ExRefType Var) IL' where
  cached (ExRefTypeI i) =
    do st <- St.get
       case find i $ extract st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- T.label (Text.unpack i) <$> C.freshVar (Text.unpack i)
                       St.modify' (`by` add i newSym)
                       return (Ref' . SI $ newSym)

  cached (ExRefTypeD d) =
    do st <- St.get
       case find d $ extract st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- T.label (Text.unpack d) <$> C.freshVar (Text.unpack d)
                       St.modify' (`by` add d newSym)
                       return $! Ref' $ SD newSym

instance Constrainable PreSolver Var IL where
  cached ref   = do st <- St.get
                    case find ref $ extract st of
                      Just x  -> return (Ref x)
                      Nothing -> do newSym <- T.sBool (Text.unpack ref)
                                    St.modify' (`by` add ref newSym)
                                    return (Ref newSym)

instance Constrainable PreSolver (ExRefType Var) IL' where
  cached (ExRefTypeI i) =
    do st <- St.get
       case find i $ extract st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- T.sInteger (Text.unpack i)
                       St.modify' (`by` add i newSym)
                       return (Ref' . SI $ newSym)

  cached (ExRefTypeD d) =
    do st <- St.get
       case find d $ extract st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- T.sDouble (Text.unpack d)
                       St.modify' (`by` add d newSym)
                       return $! Ref' $ SD newSym




----------------------------------- IL -----------------------------------------
type BRef = T.SBool

data NRef = SI T.SInteger
    | SD T.SDouble
    deriving Show

instance Num NRef where
  fromInteger = SI . T.literal . fromInteger

  abs (SI i) = SI $ abs i
  abs (SD d) = SD $ T.fpAbs d

  negate (SI i) = SI $ negate i
  negate (SD d) = SD $ T.fpNeg d

  signum (SI i) = SI $ signum i
  signum (SD d) = SD $ signum (T.fromSDouble T.sRoundNearestTiesToAway d)

  (SI i) + (SI i') = SI $ i + i'
  (SD d) + (SI i)  = SD $ d + T.sFromIntegral i
  (SI i) + (SD d)  = SD $ T.sFromIntegral i + d
  (SD d) + (SD d') = SD $ d + d'

  (SI i) - (SI i') = SI $ i - i'
  (SD d) - (SI i)  = SD $ d - T.sFromIntegral i
  (SI i) - (SD d)  = SD $ T.sFromIntegral i - d
  (SD d) - (SD d') = SD $ d - d'

  (SI i) * (SI i') = SI $ i * i'
  (SD d) * (SI i)  = SD $ d * T.sFromIntegral i
  (SI i) * (SD d)  = SD $ d * T.sFromIntegral i
  (SD d) * (SD d') = SD $ d * d'

instance PrimN NRef where
  (SI i) ./ (SI i') = SI $ i ./ i'
  (SD d) ./ (SI i)  = SD $ d ./ T.sFromIntegral i
  (SI i) ./ (SD d)  = SD $ T.sFromIntegral i ./ d
  (SD d) ./ (SD d') = SD $ d ./ d'


  (SI i) .% (SI i') = SI $ i .% i'
  (SD d) .% (SI i)  = SI $ T.fromSDouble T.sRoundNearestTiesToAway d .% i
  (SI i) .% (SD d)  = SI $ i .% T.fromSDouble T.sRoundNearestTiesToAway d
  (SD d) .% (SD d') = SD $ T.fpRem d d'

instance T.Mergeable NRef where
  symbolicMerge _ b thn els
    | Just res <- T.unliteral b = if res then thn else els
    | otherwise = els

instance T.EqSymbolic NRef where
  (.==) (SI i) (SI i') = (T..==) i i'
  (.==) (SD d) (SI i') = (T..==) d (T.sFromIntegral i')
  (.==) (SI i) (SD d)  = (T..==) (T.sFromIntegral i) d
  (.==) (SD d) (SD d') = (T..==) d d'

  (./=) (SI i) (SI i') = (T../=) i i'
  (./=) (SD d) (SI i') = (T../=) d (T.sFromIntegral i')
  (./=) (SI i) (SD d)  = (T../=) (T.sFromIntegral i) d
  (./=) (SD d) (SD d') = (T../=) d d'

instance T.OrdSymbolic NRef where
  (.<) (SI i) (SI i') = (T..<) i i'
  (.<) (SD d) (SI i)  = (T..<) d (T.sFromIntegral i)
  (.<) (SI i) (SD d)  = (T..<) (T.sFromIntegral i) d
  (.<) (SD d) (SD d') = (T..<) d d'

  (.<=) (SI i) (SI i') = (T..<=) i i'
  (.<=) (SD d) (SI i)  = (T..<=) d (T.sFromIntegral i)
  (.<=) (SI i) (SD d)  = (T..<=) (T.sFromIntegral i) d
  (.<=) (SD d) (SD d') = (T..<=) d d'

  (.>=) (SI i) (SI i') = (T..>=) i i'
  (.>=) (SD d) (SI i)  = (T..>=) d (T.sFromIntegral i)
  (.>=) (SI i) (SD d)  = (T..>=) (T.sFromIntegral i) d
  (.>=) (SD d) (SD d') = (T..>=) d d'

  (.>) (SI i) (SI i') = (T..>) i i'
  (.>) (SD d) (SI i)  = (T..>) d (T.sFromIntegral i)
  (.>) (SI i) (SD d)  = (T..>) (T.sFromIntegral i) d
  (.>) (SD d) (SD d') = (T..>) d d'

instance Prim T.SBool NRef where
  (.<) (SI i) (SI i') = (T..<) i i'
  (.<) (SD d) (SI i)  = (T..<) d (T.sFromIntegral i)
  (.<) (SI i) (SD d)  = (T..<) (T.sFromIntegral i) d
  (.<) (SD d) (SD d') = (T..<) d d'

  (.<=) (SI i) (SI i') = (T..<=) i i'
  (.<=) (SD d) (SI i)  = (T..<=) d (T.sFromIntegral i)
  (.<=) (SI i) (SD d)  = (T..<=) (T.sFromIntegral i) d
  (.<=) (SD d) (SD d') = (T..<=) d d'

  (.>=) (SI i) (SI i') = (T..>=) i i'
  (.>=) (SD d) (SI i)  = (T..>=) d (T.sFromIntegral i)
  (.>=) (SI i) (SD d)  = (T..>=) (T.sFromIntegral i) d
  (.>=) (SD d) (SD d') = (T..>=) d d'

  (.>) (SI i) (SI i') = (T..>) i i'
  (.>) (SD d) (SI i)  = (T..>) d (T.sFromIntegral i)
  (.>) (SI i) (SD d)  = (T..>) (T.sFromIntegral i) d
  (.>) (SD d) (SD d') = (T..>) d d'

  (.==) (SI i) (SI i') = (T..==) i i'
  (.==) (SD d) (SI i') = (T..==) d (T.sFromIntegral i')
  (.==) (SI i) (SD d)  = (T..==) (T.sFromIntegral i) d
  (.==) (SD d) (SD d') = (T..==) d d'

  (./=) (SI i) (SI i') = (T../=) i i'
  (./=) (SD d) (SI i') = (T../=) d (T.sFromIntegral i')
  (./=) (SI i) (SD d)  = (T../=) (T.sFromIntegral i) d
  (./=) (SD d) (SD d') = (T../=) d d'

-- | The intermediate language, we express negation but negation will not exist
-- in a variational core. The IL language is intermediate and used to collapse
-- the input program to it's essential variational structure. We exploit the
-- symbolic references to represent plain sub-trees such that the only thing
-- that will survive in the variational core are binary connectives, symbolic
-- references and choices
data IL = Unit
    | Ref BRef
    | BOp B_B IL
    | BBOp BB_B IL IL
    | IBOp NN_B IL' IL'
    | Chc Dim Proposition Proposition
    deriving Show

data IL' = Ref' NRef
    | IOp N_N IL'
    | IIOp NN_N IL' IL'
    | Chc' Dim NExpression NExpression
    deriving Show

-- | A simple tree rotation helper function. This is pivotal because as we begin
-- to remove choices we must ensure that each binary connective remains intact,
-- this means that we cannot perform arbitrary recursion and thus must rotate
-- the tree until we find a choice
rotate :: IL -> IL
-- TODO figure out if we need to keep Impl in the IL
-- rotate (BBOp Impl l r) = rotate (BBOp Or (BOp Not l) r)
rotate (BBOp And (BBOp And c@Chc{} r) r') = BBOp And c (BBOp And r r')
rotate (BBOp And (BBOp And l c@Chc{}) r')   = BBOp And c (BBOp And l r')
rotate (BBOp Or (BBOp Or c@Chc{} r) r')   = BBOp Or  c (BBOp Or r r')
rotate (BBOp Or (BBOp Or l c@Chc{}) r')     = BBOp Or  c (BBOp Or l r')
  -- TODO These rotations could be costly, benchmark them
rotate (BBOp Or (BBOp And q r) p) = BBOp And (BBOp Or p q) (BBOp Or p r)
rotate (BBOp And (BBOp Or q r) p) = BBOp Or (BBOp And p q) (BBOp And p r)
  -- General rotations
rotate (BBOp Or (BBOp Or l r) r') = BBOp Or l (BBOp Or r r')
rotate (BBOp And (BBOp And l r) r') = BBOp And l (BBOp And r r')
rotate x = x

rotate' :: IL' -> IL'
rotate' (IIOp opOuter (IIOp opInner l r) r') = IIOp opInner l (IIOp opOuter r r')
rotate' x = x

-- TODO: factor out the redundant cases into a type class
-- | Convert a proposition into the intermediate language to generate a
-- Variational Core
toIL :: ( St.MonadState State m
        , T.MonadSymbolic m
        , C.MonadQuery m
        , MonadLogger m
        , Constrainable m Var IL
        , Constrainable m (ExRefType Var) IL'
        ) => Proposition -> m IL
toIL (LitB True)   = return $! Ref T.sTrue
toIL (LitB False)  = return $! Ref T.sFalse
toIL (RefB ref)    = cached ref
toIL (OpB op e)    = BOp op <$> toIL e
toIL (OpBB Impl l r) = toIL (OpBB Or (OpB Not l) r)
toIL (OpBB Eqv l r)  = toIL (OpBB And l' r')
  where l' = bnot l ||| r -- l ==> r
        r' = bnot r ||| l -- r ==> l
toIL (OpBB XOr l r) = toIL (OpBB And l' r')
  where l' = l ||| r
        r' = bnot (l &&& r)
toIL (OpBB And l r) = do l' <- toIL  l; r' <- toIL r;  return $ BBOp And l' r'
toIL (OpBB Or l r) = do l' <- toIL  l; r' <- toIL r;  return $ BBOp Or l' r'
toIL (OpIB op l r) = do l' <- toIL' l; r' <- toIL' r; return $ IBOp op l' r'
toIL (ChcB d l r)  = return $ Chc d l r

toIL' :: ( T.MonadSymbolic m
         , St.MonadState State m
         , C.MonadQuery m
         , Constrainable m (ExRefType Var) IL'
         ) => NExpression -> m IL'
toIL' (LitI (I i))  = return . Ref' . SI $ T.literal i
toIL' (LitI (D d))  = return . Ref' . SD $ T.literal d
toIL' (RefI a)      = cached a
toIL' (OpI op e)    = IOp op <$> toIL' e
toIL' (OpII op l r) = do l' <- toIL' l; r' <- toIL' r; return $! IIOp op l' r'
toIL' (ChcI d l r)  = return $ Chc' d l r


-- TODO fix this redundancy with cached
toILSymbolic :: Proposition -> PreSolver IL
toILSymbolic (LitB True)   = return $! Ref T.sTrue
toILSymbolic (LitB False)  = return $! Ref T.sFalse
toILSymbolic (RefB ref)    = cached ref
toILSymbolic (OpB op e)    = BOp op <$> toILSymbolic e
toILSymbolic (OpBB Impl l r) = toILSymbolic (OpBB Or (OpB Not l) r)
toILSymbolic (OpBB Eqv l r)  = toILSymbolic (OpBB And l' r')
  where l' = bnot l ||| r -- l ==> r
        r' = bnot r ||| l -- r ==> l
toILSymbolic (OpBB XOr l r) = toILSymbolic (OpBB And l' r')
  where l' = l ||| r
        r' = bnot (l &&& r)
toILSymbolic (OpBB And l r) = do l' <- toILSymbolic  l
                                 r' <- toILSymbolic r
                                 return $ BBOp And l' r'
toILSymbolic (OpBB Or l r) = do l' <- toILSymbolic  l
                                r' <- toILSymbolic r
                                return $ BBOp Or l' r'
toILSymbolic (OpIB op l r) = do l' <- toILSymbolic' l; r' <- toILSymbolic' r; return $ IBOp op l' r'
toILSymbolic (ChcB d l r)  = return $ Chc d l r

toILSymbolic' :: NExpression -> PreSolver IL'
toILSymbolic' (LitI (I i))  = return . Ref' . SI $ T.literal i
toILSymbolic' (LitI (D d))  = return . Ref' . SD $ T.literal d
toILSymbolic' (RefI a)      = cached a
toILSymbolic' (OpI op e)    = IOp op <$> toILSymbolic' e
toILSymbolic' (OpII op l r) = do l' <- toILSymbolic' l
                                 r' <- toILSymbolic' r
                                 return $! IIOp op l' r'
toILSymbolic' (ChcI d l r)  = return $ Chc' d l r

-------------------------------- Accumulation -----------------------------------
-- For both evaluation and accumulation we implement the functions in a verbose
-- way to aid the code generator. This is likely not necessary but one missed
-- INLINE could mean a large decrease in performance, because evaluation and
-- accumulation are both extremely hot code we want to make them as fast as
-- possible

-- | A variational core is a partially evaluated AST in the IL language. The
-- goal is to reduce as much as possible all plain terms leaving only symbolic
-- references, choices and logical connectives
newtype VarCore = VarCore {getCore :: IL}
  deriving Show

-- | Helper function to wrap an IL into a variational core
intoCore :: IL -> VarCore
intoCore = VarCore

isUnit :: VarCore -> Bool
isUnit (VarCore Unit) = True
isUnit _              = False

-- | drive negation down to the leaves as much as possible
driveNotDown :: IL -> IL
driveNotDown (Ref b)     = Ref (bnot b)
driveNotDown (BOp Not e) = e
driveNotDown (BBOp And l r) = BBOp Or (driveNotDown l) (driveNotDown r)
driveNotDown (BBOp Or l r) = BBOp And (driveNotDown l) (driveNotDown r)
driveNotDown (BBOp XOr l r) = driveNotDown p
  where p = BBOp Or (BBOp And l (driveNotDown r)) (BBOp And (driveNotDown l) r)
driveNotDown (BBOp Impl l r) = BBOp And l (driveNotDown r)
driveNotDown (BBOp Eqv l r) = BBOp Eqv l (driveNotDown r)
driveNotDown (IBOp EQ l r)  = IBOp NEQ l r
driveNotDown (IBOp NEQ l r) = IBOp EQ l r
driveNotDown (IBOp op l r)  = IBOp op r l -- 1 < 2 === -(2 < 1)
driveNotDown (Chc d l r)    = Chc d (bnot l) (bnot r)

driveNotDown Unit = error "Not applied to a Unit!"

-- | Accumulation: we purposefully are verbose to provide the optimizer better
-- opportunities. Accumulation seeks to combine as much as possible the plain
-- terms in the AST into symbolic references
accumulate :: IL -> IL
 -- computation rules
accumulate Unit                        = Unit
  -- bools
accumulate (BOp Not (Ref r))           = Ref $! bnot r
accumulate (BBOp And (Ref l) (Ref r))  = Ref $! l &&& r
accumulate (BBOp Or  (Ref l) (Ref r))  = Ref $! l ||| r
  -- numerics
accumulate (IBOp LT (Ref' l) (Ref' r))  = Ref $! l .< r
accumulate (IBOp LTE (Ref' l) (Ref' r)) = Ref $! l .<= r
accumulate (IBOp EQ (Ref' l) (Ref' r))  = Ref $! l .== r
accumulate (IBOp NEQ (Ref' l) (Ref' r)) = Ref $! l ./= r
accumulate (IBOp GT (Ref' l) (Ref' r))  = Ref $! l .>  r
accumulate (IBOp GTE (Ref' l) (Ref' r)) = Ref $! l .>= r
  -- choices
accumulate x@(BBOp _ Chc {} Chc {})    = x
accumulate x@(BBOp _ (Ref _) Chc {})   = x
accumulate x@(BBOp _ Chc {} (Ref _))   = x
accumulate x@(IBOp _ Chc' {} Chc' {})  = x
accumulate x@(IBOp _ (Ref' _) Chc' {}) = x
accumulate x@(IBOp _ Chc' {} (Ref' _)) = x

 -- congruence rules
accumulate (BOp Not e)   = accumulate $ driveNotDown e
accumulate (BBOp op l r) = let l' = accumulate l
                               r' = accumulate r
                               res = BBOp op l' r' in
                             if isValue l' && isValue r'
                             then accumulate res
                             else res
accumulate (IBOp op l r) = accumulate $! IBOp op (accumulate' l) (accumulate' r)
accumulate x = x

accumulate' :: IL' -> IL'
  -- computation rules
accumulate' x@(Ref' _)          = x
accumulate' (IOp Neg  (Ref' n)) = Ref' $! negate n
accumulate' (IOp Abs  (Ref' n)) = Ref' $! abs n
accumulate' (IOp Sign (Ref' n)) = Ref' $! signum n
accumulate' (IIOp Add (Ref' l) (Ref' r))  = Ref' $! l + r
accumulate' (IIOp Sub (Ref' l) (Ref' r))  = Ref' $! l - r
accumulate' (IIOp Mult (Ref' l) (Ref' r)) = Ref' $! l * r
accumulate' (IIOp Div (Ref' l) (Ref' r))  = Ref' $! l ./ r
accumulate' (IIOp Mod (Ref' l) (Ref' r))  = Ref' $! l .% r
  -- choices
accumulate' x@Chc' {}                     = x
accumulate' (IOp op (Chc' d l r))         = Chc' d (OpI op l) (OpI op r)
accumulate' x@(IIOp _ Chc' {} Chc' {})    = x
accumulate' x@(IIOp _ (Ref' _) Chc' {})   = x
accumulate' x@(IIOp _ Chc' {} (Ref' _))   = x
accumulate' (IIOp op c@Chc'{} r)   = IIOp op c $ accumulate' r
accumulate' (IIOp op l c@Chc'{})   = IIOp op (accumulate' l) c
  -- congruence rules
accumulate' (IOp o e)  = accumulate' $ IOp o (accumulate' e)
accumulate' x@(IIOp o l r) = let l'  = accumulate' l
                                 r'  = accumulate' r
                                 res = IIOp o l' r' in
                               trace ("rec ---> " ++ show x ++ "\n") $
                               -- this check is required or else we may
                               -- infinitely recur on edge cases with choices
                               -- TODO encode the property in the type system to
                               -- avoid the check
                               if isValue' l' && isValue' r'
                               then trace ("reducing :: " ++ show x) accumulate' res
                               else trace ("Done reducing ::  " ++ show (pretty res)) res

-------------------------------- Evaluation -----------------------------------
toSolver :: (Monad m, SolverContext m) => T.SBool -> m VarCore
toSolver a = do constrain a; return $! intoCore Unit

isValue :: IL -> Bool
isValue Unit    = True
isValue (Ref _) = True
isValue _       = False

isValue' :: IL' -> Bool
isValue' (Ref' _) = True
isValue' _        = False

-- | Evaluation will remove plain terms when legal to do so, "sending" these
-- terms to the solver, replacing them to Unit to reduce the size of the
-- variational core
evaluate :: IL -> Solver VarCore
  -- computation rules
evaluate Unit     = do s <- St.get
                       logWith "Received Unit with State: " s
                       return $! intoCore Unit
evaluate (Ref b)  = toSolver b
evaluate x@Chc {} = return $! intoCore x
  -- bools
evaluate (BOp Not   (Ref r))         = toSolver $! bnot r
evaluate (BBOp And  (Ref l) (Ref r)) = toSolver $! l &&& r
evaluate (BBOp Or   (Ref l) (Ref r)) = toSolver $! l ||| r
  -- numerics
evaluate (IBOp LT  (Ref' l) (Ref' r)) = toSolver $! l .< r
evaluate (IBOp LTE (Ref' l) (Ref' r)) = toSolver $! l .<= r
evaluate (IBOp EQ  (Ref' l) (Ref' r)) = toSolver $! l .== r
evaluate (IBOp NEQ (Ref' l) (Ref' r)) = toSolver $! l ./= r
evaluate (IBOp GT  (Ref' l) (Ref' r)) = toSolver $! l .>  r
evaluate (IBOp GTE (Ref' l) (Ref' r)) = toSolver $! l .>= r
  -- choices
evaluate x@(BBOp _ Chc {}  Chc {})   = return $! intoCore x
evaluate x@(BBOp _ (Ref _) Chc {})   = return $! intoCore x
evaluate x@(BBOp _ Chc {} (Ref _))   = return $! intoCore x
evaluate x@(IBOp _ Chc' {} Chc' {})  = return $! intoCore x
evaluate x@(IBOp _ (Ref' _) Chc' {}) = return $! intoCore x
evaluate x@(IBOp _ Chc' {} (Ref' _)) = return $! intoCore x
  -- congruence cases
evaluate (BBOp And l Unit)      = evaluate l
evaluate (BBOp And Unit r)      = evaluate r
evaluate (BBOp And l x@(Ref _)) = do _ <- evaluate x; evaluate l
evaluate (BBOp And x@(Ref _) r) = do _ <- evaluate x; evaluate r
evaluate x@(IBOp op l r)        =
  logWith "Found relation" x >>
  let l' = accumulate' l
      r' = accumulate' r
      res = IBOp op l' r' in
    if isValue' l' && isValue' r'
    then log "Trying to reduce relation" >> evaluate res
    else return $! intoCore res


  -- accumulation cases
evaluate x@(BOp Not _)  = evaluate $ accumulate x
evaluate x@(BBOp And l r) = logWith "Eval And Recurring: " x >>
  do (VarCore l') <- evaluate l
     (VarCore r') <- evaluate r
     let res = BBOp And l' r'
     -- this is a hot loop, but we want to make the
     -- variational core as small as possible because it
     -- will pay off in the solver. Thus we perform a
     -- check here to determine if we can reduce the
     -- variational core even after a single pass
     if isValue l' || isValue r'
       then evaluate res
       else return $! intoCore res
evaluate x@(BBOp op l r)  = logWith "Eval General Recurring " x >>
  let l' = accumulate l
      r' = accumulate r
      res = BBOp op l' r' in
    if isValue l' && isValue r'
       then logWith "Reducing more" res >> evaluate res
    else do logWith "couldn't reduce" res
            logWith "Left " l'
            logWith "Right " r'
            return $! intoCore res

------------------------- Removing Choices -------------------------------------
-- | We use a zipper to track the context when searching for choices, this
-- removes the need to perform tree rotations
data Ctx = InL Ctx BB_B IL -- ^ In lhs, Ctx' is parent node, by op, with right
         | InR T.SBool BB_B Ctx
         | InLB Ctx  NN_B IL'
         | InRB NRef NN_B Ctx
         | InL' Ctx  NN_N IL'
         | InR' NRef NN_N Ctx
         | Top
         deriving Show

data Loc = InBool IL  Ctx
         | InNum  IL' Ctx
         deriving Show

toLoc :: IL -> Loc
toLoc = toLocWith Top

toLoc' :: IL' -> Loc
toLoc' = toLocWith' Top

toLocWith :: Ctx -> IL -> Loc
toLocWith = flip InBool

toLocWith' :: Ctx -> IL' -> Loc
toLocWith' = flip InNum

findChoice :: Loc -> Loc
  -- base cases
findChoice x@(InBool Ref{} Top) = x
findChoice x@(InBool Unit Top)  = x
findChoice x@(InBool Chc {} _)  = x
findChoice x@(InNum Chc' {} _)  = x
  -- discharge two references
findChoice (InBool l@Ref {} (InL parent op r@Ref {}))   = findChoice (InBool (accumulate  $ BBOp op l r) parent)
findChoice (InNum l@Ref' {} (InLB parent op r@Ref' {})) = findChoice (InBool (accumulate  $ IBOp op l r) parent)
findChoice (InNum l@Ref' {} (InL' parent op r@Ref' {})) = findChoice (InNum  (accumulate' $ IIOp op l r) parent)
  -- fold
findChoice (InBool r@Ref {} (InR acc op parent))  =
  findChoice (InBool (accumulate $ BBOp op (Ref acc) r) parent)
findChoice (InNum r@Ref' {} (InRB acc op parent)) =
  findChoice (InBool (accumulate $ IBOp op (Ref' acc) r) parent)
findChoice (InNum r@Ref' {} (InR' acc op parent)) =
  findChoice (InNum (accumulate' $ IIOp op (Ref' acc) r) parent)
  -- switch
findChoice (InBool (Ref l) (InL parent op r))   = findChoice (InBool r $ InR l op parent)
findChoice (InNum  (Ref' l) (InLB parent op r)) = findChoice (InNum  r $ InRB l op parent)
findChoice (InNum  (Ref' l) (InL' parent op r)) = findChoice (InNum  r $ InR' l op parent)
  -- recur
findChoice (InBool (BBOp op l r) ctx) = findChoice (InBool l $ InL ctx op r)
findChoice (InBool (IBOp op l r) ctx) = findChoice (InNum  l $ InLB ctx op r)
findChoice (InNum (IIOp op l r) ctx) = findChoice (InNum  l $ InL' ctx op r)
  -- legal to discharge Units under a conjunction only
findChoice (InBool Unit (InL parent And r)) = findChoice (InBool r $ InR true And parent)
findChoice (InBool Unit (InR acc And parent)) = findChoice (InBool (Ref acc) parent)
findChoice (InBool BOp {} _) = error "Can't have unary Ops in IL!!!"
findChoice (InNum IOp {} _) = error "Can't have unary Ops in IL!!!"
  -- TODO Not sure if unit can ever exist with a context
findChoice (InBool Unit ctx) = error $ "Unit with a context" ++ show ctx
findChoice x@(InNum Ref'{} Top)    = error $ "Numerics can only exist in SMT within a relation: " ++ show x
findChoice x@(InBool Ref{} InLB{}) = error $ "An impossible case bool reference in inequality: "  ++ show x
findChoice x@(InBool Ref{} InRB{}) = error $ "An impossible case bool reference in inequality: "  ++ show x
findChoice x@(InBool Ref{} InL'{}) = error $ "An impossible case bool reference in arithmetic: " ++ show x
findChoice x@(InBool Ref{} InR'{}) = error $ "An impossible case bool reference in arithmetic: "  ++ show x
findChoice x@(InNum Ref'{} InL{}) = error $ "An impossible case: " ++ show x
findChoice x@(InNum Ref'{} InR{}) = error $ "An impossible case: " ++ show x



store :: Result -> Solver ()
store = St.modify' . flip by . (<>)

-- | TODO newtype this maybe stuff, this is an alternative instance
onVContext :: Maybe VariantContext -> Maybe VariantContext -> Maybe VariantContext
onVContext Nothing Nothing = Nothing
onVContext a@(Just _) Nothing = a
onVContext Nothing b@(Just _) = b
onVContext (Just l) (Just r)  = Just $ l &&& r

-- | A function that enforces each configuration is updated in sync
updateConfigs :: (St.MonadState State m) => SVariantContext -> Prop' Dim -> (Dim, Bool) -> m ()
updateConfigs conf context (d,val) = do
  St.modify' (`by` flip (&&&) conf)
  St.modify' (`by` (`onVContext` (Just $ VariantContext context)))
  St.modify' (`by` add d val)

-- | Reset the state but maintain the cache's
resetTo :: (St.MonadState State m) => State -> m ()
resetTo s = do
  st <- St.get
  St.put s{ result=result st
          , bools=bools st
          , ints=ints st
          , doubles=doubles st
          , dimensions=dimensions st
          }

-- | Given a dimensions and a way to continue with the left alternative, and a
-- way to continue with the right alternative. Spawn two new subprocesses that
-- process the alternatives plugging the choice hole with its respective
-- alternatives
alternative ::
     ( St.MonadState State m
     , MonadLogger m
     , MonadIO m
     , C.MonadQuery m
     , Constrainable m Dim T.SBool)
  => Dim -> m () -> m () -> m ()
alternative dim goLeft goRight =
  do conf <- St.gets config
     case find dim conf of
       Just True  -> log "Left Selected"  >> goLeft
       Just False -> log "Right Selected" >> goRight
       Nothing -> -- then this is a new dimension
         do
           sD <- cached dim
           s <- St.get -- cache the state

         -- left side
           C.inNewAssertionStack $
             do log "Left Alternative"
                updateConfigs sD (bRef dim) (dim,True)
                goLeft

           -- reset for left side
           resetTo s

           -- right side
           C.inNewAssertionStack $
             do log "Right Alternative"
                updateConfigs (bnot sD) (bnot $ bRef dim) (dim,False)
                goRight


removeChoices :: VarCore -> Solver ()
  -- singleton instances
removeChoices (VarCore Unit) = log "Core reduced to Unit" >>
                               St.get >>= getResult . vConfig >>= store
removeChoices (VarCore x@(Ref _)) = evaluate x >>= removeChoices
removeChoices (VarCore BOp {}) = error "How did a negation get into var core?"
removeChoices (VarCore l) = choose (toLoc l)


choose :: Loc -> Solver ()
choose (InBool l@Ref{} Top) = evaluate l >>= removeChoices
choose loc =
  do
    let loc' = findChoice loc
    case loc' of
      x@(InBool (Chc d cl cr) ctx) -> do
        logWith "Got choice in context: " x
        conf <- St.gets config
        let goLeft  = toIL cl >>= choose . findChoice . toLocWith ctx . accumulate
            goRight = toIL cr >>= choose . findChoice . toLocWith ctx . accumulate

        case find d conf of
          Just True  -> goLeft
          Just False -> goRight
          Nothing    -> alternative d goLeft goRight

      x@(InNum (Chc' d cl cr) ctx) -> do
        logWith "Got choice in context: " x
        conf <- St.gets config
        let goLeft  = toIL' cl >>= choose . findChoice . toLocWith' ctx . accumulate'
            goRight = toIL' cr >>= choose . findChoice . toLocWith' ctx . accumulate'

        case find d conf of
          Just True  -> goLeft
          Just False -> goRight
          Nothing    -> alternative d goLeft goRight

      (InBool Unit Top) -> removeChoices $ intoCore Unit
      x -> error $ "Choosing and missed cases with: " ++ show x


-- choose (VarCore (Chc d l r)) =
--   do
--     log "singleton choice"
--     conf <- St.gets config
--     let goLeft  = toIL l >>= evaluate >>= choose
--         goRight = toIL r >>= evaluate >>= choose
--     case find d conf of
--       Just True  -> log "left"  >> goLeft
--       Just False -> log "right" >> goRight
--       Nothing    -> alternative d goLeft goRight -- then this is a new dimension


-- choose (VarCore (BBOp op (Chc d cl cr) r)) =
--   do
--     log "Choice in left"
--     conf <- St.gets config
--     let goLeft  = toIL cl >>= evaluate . (\x -> BBOp op x r) >>= choose
--         goRight = toIL cr >>= evaluate . (\x -> BBOp op x r) >>= choose
--      -- check if d is in the configuration, and if so what is its value
--     case find d conf of
--        Just True  -> goLeft
--        Just False -> goRight
--        Nothing    -> alternative d goLeft goRight


-- choose (VarCore (BBOp op l (Chc d cl cr))) =
--   do
--     log "Choice in Right"
--     conf <- St.gets config
--     let goLeft  = toIL cl >>= evaluate . BBOp op l >>= choose
--         goRight = toIL cr >>= evaluate . BBOp op l >>= choose
--      -- check if d is in the configuration, and if so what is its value
--     case find d conf of
--        Just True  -> goLeft
--        Just False -> goRight
--        Nothing    -> alternative d goLeft goRight

--   -- Arithmetic
-- choose (VarCore (IBOp op (Chc' d cl cr) r)) =
--   do
--     conf <- St.gets config
--     let goLeft  = toIL' cl >>= evaluate . (\x -> IBOp op x r) >>= choose
--         goRight = toIL' cr >>= evaluate . (\x -> IBOp op x r) >>= choose
--     case find d conf of
--       Just True  -> goLeft
--       Just False -> goRight
--       Nothing    -> alternative d goLeft goRight

-- choose (VarCore (IBOp op l (Chc' d cl cr))) =
--   do
--     conf <- St.gets config
--     let goLeft  = toIL' cl >>= evaluate . IBOp op l >>= choose
--         goRight = toIL' cr >>= evaluate . IBOp op l >>= choose
--     case find d conf of
--       Just True  -> goLeft
--       Just False -> goRight
--       Nothing    -> alternative d goLeft goRight

-- choose (VarCore (IBOp op l r)) = do log "arith recurrence"
--                                     choose' op (toLoc' l) (toLoc' r)
-- choose (VarCore x@BBOp {}) =
--   -- when choices do not appear in the child of the root node we must rotate the
--   -- AST such that the the binary relation at the root is preserved
--   do logPretty "zippering: " x

-- choose (VarCore BOp {}) = error "Impossible occurred: received a Not in a variational core!!"

---------------------- Removing Choices in Arithmetic --------------------------
-- | A zipper context that can only fold from the left
-- data Ctx' = InL' Ctx' NN_N IL' -- ^ In lhs, Ctx' is parent node, by op, with right child IL
--          | InR' NRef NN_N Ctx'
--          | Top'
--          deriving Show

-- type Loc' = (IL', Ctx')

-- toLoc' :: IL' -> Loc'
-- toLoc' x = (x, Top')

-- | Find the values and return the value as the focus with context
-- findChoice' :: Loc -> Loc
-- findChoice' x@(Ref'{}, Top') = x -- done
-- findChoice' x@(Chc' {}, _) = x -- done
-- findChoice' (IIOp op l r, Top') = findChoice' (l, InL' Top' op r) -- drive down to the left
--   -- discharge two references
-- findChoice' (l@Ref' {}, InL' parent op r@Ref' {}) = findChoice' (accumulate' $ IIOp op l r, parent)
--   -- fold
-- findChoice' (r@Ref' {}, InR' acc op parent) = findChoice' (accumulate' $ IIOp op (Ref' acc) r, parent)
--   -- switch
-- findChoice' (Ref' l, InL' parent op r) = findChoice' (r, InR' l op parent)
--   -- recur
-- findChoice' (IIOp op l r, ctx) = findChoice' (l, InL' ctx op r)
-- findChoice' (IOp {}, _) = error "Can't have unary Ops in IL'!!!"

-- | Here we need to detect choices that cannot be rotated up the tree due to
-- the hard barrier between arithmetic and booleans, i.e., that arithmetic _can
-- only_ exist in the context of an operator that concludes to a boolean. Thus,
-- we capture the outer context, recur down to the first choice and then call
-- back up to the boolean solver. This is effectively a zipper encoded as a
-- function.
-- choose' :: NN_B -> Loc -> Loc -> Solver IL
-- choose' = undefined
-- choose' rootOp (x@Ref' {}, Top') (y@Ref' {}, Top') = return $ accumulate (IBOp rootOp x y)
-- choose' rootOp lhs rhs =
--   do
--   let l' = findChoice' lhs
--       r' = findChoice' rhs
--   case (l', r') of
--     ((Chc' d cl cr, ctx), rhs') ->
--       do
--         conf <- St.gets config
--         let goLeft  = toIL' cl >>= (\x -> choose' rootOp x rhs') . (\x -> findChoice' (x,ctx)) . accumulate'
--             goRight = toIL' cr >>= (\x -> choose' rootOp x rhs') . (\x -> findChoice' (x,ctx)) . accumulate'

--         case find d conf of
--           Just True  -> goLeft
--           Just False -> goRight
--           Nothing    -> alternative d goLeft goRight

--     (lhs', (Chc' d cl cr, ctx)) ->
--       do
--         conf <- St.gets config
--         let goLeft  = toIL' cl >>= choose' rootOp lhs' . findChoice' . (,ctx) . accumulate'
--             goRight = toIL' cr >>= choose' rootOp lhs' . findChoice' . (,ctx) . accumulate'

--         case find d conf of
--           Just True  -> goLeft
--           Just False -> goRight
--           Nothing    -> alternative d goLeft goRight


--     a -> error $ "Error in Arithmetic Zipper, function choose': " ++ show a

--------------------------- Variant Context Helpers ----------------------------
contextToSBool :: (Has Dimensions, S.MonadSymbolic m, St.MonadState State m, MonadLogger m) =>
  VariantContext -> m T.SBool
contextToSBool (getVarFormula -> x) = go x
  where -- go :: Show a => Prop' a -> m T.SBool
        go (LitB True)  = return S.sTrue
        go (LitB False) = return S.sFalse
        go (RefB d)     = do ds <- St.gets dimensions
                             case find d ds of
                               Just d' -> return d' -- then we've seen it before
                               Nothing -> do
                                 newSym <- T.label d <$> T.sBool d
                                 logWith "making symb" d
                                 St.modify' (`by` add d newSym)
                                 return $! newSym
        go (OpB Not e) = bnot <$> go e
        go (OpBB op l r) = do l' <- go l
                              r' <- go r
                              let op' = dispatch op
                              return $ l' `op'` r'
        go OpIB {} = error "numeric expressions are invalid in variant context"
        go ChcB {} = error "variational expressions are invalid in variant context"

        dispatch And  = (&&&)
        dispatch Or   = (|||)
        dispatch Impl = (==>)
        dispatch Eqv  = (<=>)
        dispatch XOr  = (<=>)

instance Pretty IL where
  pretty Unit = "unit"
  pretty (Ref b)      = pretty b
  pretty (BOp Not r@(Ref _))   = "~" <> pretty r
  pretty (BOp o e)   = pretty o <> parens (pretty e)
  pretty (BBOp op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (IBOp op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (Chc d l r)  = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty NRef where
  pretty (SI _) = "sInt"
  pretty (SD _) = "sDouble"

instance Pretty IL' where
  pretty (Ref' b)     = pretty b
  pretty (IOp o x@(Ref' _))  = pretty o <> pretty x
  pretty (IOp Abs e)  = between "|" (pretty e) "|"
  pretty (IOp o e)  = pretty o <> parens (pretty e)
  pretty (IIOp o l r) = parens $ mconcat [pretty l, " ", pretty o, " ", pretty r]
  pretty (Chc' d l r) = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"
