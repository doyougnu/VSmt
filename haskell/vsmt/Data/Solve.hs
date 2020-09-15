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

module Data.Solve where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.Logger       (LoggingT, MonadLogger (..),
                                             logDebug, runStdoutLoggingT)
import qualified Control.Monad.State.Strict as St (MonadState, StateT, get,
                                                   gets, modify', runStateT,put)
import           Control.Monad.Trans        (MonadIO, MonadTrans, lift, liftIO)
import qualified Data.HashMap.Strict        as Map
-- import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Text                  as Text
import           Prelude                    hiding (EQ, GT, LT, log)
-- import Data.Function ((&))

import qualified Data.SBV                   as S
import           Data.SBV.Internals         (SolverContext (..))
import qualified Data.SBV.Trans             as T
import qualified Data.SBV.Trans.Control     as C

import           Data.Core.Result
import           Data.Core.Types

------------------------------ Template Haskell --------------------------------
log :: MonadLogger m => Text.Text -> m ()
log = $(logDebug)

-- TODO custom loggers for each function, i.e., [DEBUG:EVAL]: ...
logWith :: (MonadLogger m, Show a) => Text.Text -> a -> m ()
logWith msg value = log $ msg <> sep <> Text.pack (show value)
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
solve :: Proposition -> Maybe VariantContext -> IO (Result, State)
solve i vConf =
    -- (context,dims) <- runStdoutLoggingT $ St.runStateT (contextToSBool conf) mempty{config=conf}
    T.runSMTWith T.z3{T.verbose=True} $
      do let _ = fromMaybe true vConf
         (il, st) <- runStdoutLoggingT $ runSolver mempty $ toILSymbolic i
         C.query $ runStdoutLoggingT $ runSolver st $
           do core <- findVCore il
              _ <- choose core
              solution

sat :: Proposition -> Maybe VariantContext -> IO Result
sat = (fmap fst .) <$> solve

------------------------------ Data Types --------------------------------------
-- | Solver configuration is a mapping of dimensions to boolean values, we
-- express this in two ways, first we hold a store of dimensions to symbolic
-- booleans to track what we have seen, secondly we hold a variant context _as_
-- a symbolic formula rather than a data structure so that we can spin up a
-- separate thread to check variant context sat calls when removing choices
type Store = Map.HashMap

type Ints         = Store Var T.SInt32
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

instance Has VariantContext where extract     = vConfig
                                  wrap    d w = w{vConfig=d}


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
    , vConfig    :: VariantContext  -- the formula representation of the config
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
         Nothing -> do newSym <- T.sInt32 (Text.unpack i)
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

data NRef = SI T.SInt32
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
toIL (RefB ref)    = do logWith "Found" ref; cached ref
toIL (OpB op e)    = BOp op <$> toIL e
toIL (OpBB op l r) = do l' <- toIL  l; r' <- toIL r;  return $ BBOp op l' r'
toIL (OpIB op l r) = do l' <- toIL' l; r' <- toIL' r; return $ IBOp op l' r'
toIL (ChcB d l r)  = return $ Chc d l r

toIL' :: ( T.MonadSymbolic m
         , St.MonadState State m
         , C.MonadQuery m
         , Constrainable m (ExRefType Var) IL'
         ) => NExpression -> m IL'
toIL' (LitI (I i))  = Ref' . SI <$> T.sInt32 (show i)
toIL' (LitI (D d))  = Ref' . SD <$> T.sDouble (show d)
toIL' (RefI a)      = cached a
toIL' (OpI op e)    = IOp op <$> toIL' e
toIL' (OpII op l r) = do l' <- toIL' l; r' <- toIL' r; return $! IIOp op l' r'
toIL' (ChcI d l r)  = return $ Chc' d l r


-- TODO fix this redundancy
toILSymbolic :: Proposition -> PreSolver IL
toILSymbolic (LitB True)   = return $! Ref T.sTrue
toILSymbolic (LitB False)  = return $! Ref T.sFalse
toILSymbolic (RefB ref)    = do logWith "Found" ref; cached ref
toILSymbolic (OpB op e)    = BOp op <$> toILSymbolic e
toILSymbolic (OpBB op l r) = do l' <- toILSymbolic  l; r' <- toILSymbolic r;  return $ BBOp op l' r'
toILSymbolic (OpIB op l r) = do l' <- toILSymbolic' l; r' <- toILSymbolic' r; return $ IBOp op l' r'
toILSymbolic (ChcB d l r)  = return $ Chc d l r

toILSymbolic' :: NExpression -> PreSolver IL'
toILSymbolic' (LitI (I i))  = Ref' . SI <$> T.sInt32 (show i)
toILSymbolic' (LitI (D d))  = Ref' . SD <$> T.sDouble (show d)
toILSymbolic' (RefI a)      = cached a
toILSymbolic' (OpI op e)    = IOp op <$> toILSymbolic' e
toILSymbolic' (OpII op l r) = do l' <- toILSymbolic' l; r' <- toILSymbolic' r; return $! IIOp op l' r'
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
newtype VarCore = VarCore IL
  deriving Show

-- | Helper function to wrap an IL into a variational core
intoCore :: IL -> VarCore
intoCore = VarCore

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
accumulate (BBOp Impl (Ref l) (Ref r)) = Ref $! l ==> r
accumulate (BBOp Eqv  (Ref l) (Ref r)) = Ref $! l <=> r
accumulate (BBOp XOr  (Ref l) (Ref r)) = Ref $! l <+> r
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
                               r' = accumulate r in
                              accumulate (BBOp op l' r')
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
  -- congruence rules
accumulate' (IOp o e)  = accumulate' $ IOp o (accumulate' e)
accumulate' (IIOp o l r) = let l' = accumulate' l
                               r' = accumulate' r in
                             accumulate' (IIOp o l' r')

-------------------------------- Evaluation -----------------------------------
toSolver :: (Monad m, SolverContext m) => T.SBool -> m VarCore
toSolver a = do constrain a; return $! intoCore Unit

-- | Evaluation will remove plain terms when legal to do so, "sending" these
-- terms to the solver, replacing them to Unit to reduce the size of the
-- variational core
evaluate :: IL -> Solver VarCore
  -- computation rules
evaluate Unit     = do log "Unit"
                       s <- St.get
                       logWith "State: " s
                       return $! intoCore Unit
evaluate (Ref b)  = toSolver b
evaluate x@Chc {} = return $! intoCore x
  -- bools
evaluate (BOp Not   (Ref r))         = toSolver $! bnot r
evaluate (BBOp And  (Ref l) (Ref r)) = toSolver $! l &&& r
evaluate (BBOp Or   (Ref l) (Ref r)) = toSolver $! l ||| r
evaluate (BBOp Impl (Ref l) (Ref r)) = toSolver $! l ==> r
evaluate (BBOp Eqv  (Ref l) (Ref r)) = toSolver $! l <=> r
evaluate (BBOp XOr  (Ref l) (Ref r)) = toSolver $! l <+> r
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
evaluate (IBOp op l r)        = evaluate $! IBOp op (evaluate' l) (evaluate' r)

  -- accumulation cases
evaluate x@(BOp Not _)  = evaluate $ accumulate x
evaluate (BBOp And l r) = do (VarCore l') <- evaluate l
                             (VarCore r') <- evaluate r
                             evaluate $! BBOp And l' r'
evaluate (BBOp op l r)  = evaluate $! BBOp op (accumulate l) (accumulate r)


evaluate' :: IL' -> IL'
evaluate' x@(Ref' _)                    = x
evaluate' x@Chc' {}                     = x
evaluate' (IOp Neg   (Ref' n))          = Ref' $! negate n
evaluate' (IOp Abs   (Ref' n))          = Ref' $! abs n
evaluate' (IOp Sign  (Ref' n))          = Ref' $! signum n
evaluate' (IIOp Add  (Ref' l) (Ref' r)) = Ref' $! l + r
evaluate' (IIOp Sub  (Ref' l) (Ref' r)) = Ref' $! l - r
evaluate' (IIOp Mult (Ref' l) (Ref' r)) = Ref' $! l * r
evaluate' (IIOp Div  (Ref' l) (Ref' r)) = Ref' $! l ./ r
evaluate' (IIOp Mod  (Ref' l) (Ref' r)) = Ref' $! l .% r
  -- choices
evaluate' (IOp op (Chc' d l r))         = Chc' d (OpI op l) (OpI op r)
evaluate' x@(IIOp _ Chc' {} Chc' {})    = x
evaluate' x@(IIOp _ (Ref' _) Chc' {})   = x
evaluate' x@(IIOp _ Chc' {} (Ref' _))   = x
  -- accumulation rules
evaluate' (IOp o e)  = evaluate' $! IOp o (accumulate' e)
evaluate' (IIOp o l r) = let l' = accumulate' l
                             r' = accumulate' r in
                             evaluate' $! accumulate' (IIOp o l' r')

------------------------- Removing Choices -------------------------------------
store :: Result -> Solver ()
store s = do
  logWith "Model: " s
  St.modify' (`by` (<> s))
  -- St.modify' . flip by . (<>)

updateConfigs :: (St.MonadState State m) => SVariantContext -> Prop' Dim -> (Dim, Bool) -> m ()
updateConfigs conf context (d,val) = do
  St.modify' (`by` flip (&&&) conf)
  St.modify' (`by` flip (&&&) (VariantContext context))
  St.modify' (`by` add d val)

resetTo :: (St.MonadState State m) => State -> m ()
resetTo s = do
  r <- St.gets result
  St.put s{result=r}

choose :: VarCore -> Solver ()
choose (VarCore Unit) = do
  s <- St.get
  logWith "Choose Unit with State" s
  St.get >>= getResult . vConfig >>= store
choose (VarCore (Chc d l r)) =
  do conf <- St.gets config
     case find d conf of
       Just True  -> log "left"  >> toIL l >>= choose . VarCore
       Just False -> log "right" >> toIL r >>= choose . VarCore
       Nothing -> -- then this is a new dimension
         do
           sD <- T.label ("Dimension: " ++ d) <$> C.freshVar d
           s <- St.get -- cache the state

         -- left side
           C.inNewAssertionStack $
             do log "New Stack Left"
                updateConfigs sD (bRef d) (d,True)
                toIL l >>= evaluate >>= choose

           -- reset for left side
           resetTo s

           -- right side
           C.inNewAssertionStack $
             do log "New Stack Right"
                updateConfigs (bnot sD) (bnot $ bRef d) (d,False)
                toIL r >>= evaluate >>= choose


choose (VarCore (BBOp op (Chc d cl cr) r)) =
  do let l' = toIL cl
         r' = toIL cr
     sConf <- St.gets sConfig
     -- check if d is in the configuration, and if so what is its value
     T.getModelValue d <$> liftIO (S.sat sConf) >>= \case
       Just True  -> l' >>= choose . VarCore
       Just False -> r' >>= choose . VarCore
       Nothing    -> -- then we have not observed this choice before
         do let sD = C.freshVar d
                lConf = (sConf &&&)        <$> sD -- set Dim to true in conf
                rConf = (sConf &&&) . bnot <$> sD -- Dim is false in conf
            -- left side
            C.inNewAssertionStack $
              do
                St.modify' (`by` add d True)
                lConf >>= St.modify' . wrap -- set conf to new conf with new choice
                St.modify' (`by` (&&&) (VariantContext $ bRef d))
                (\x -> BBOp op x r) <$> l' >>= evaluate >>= choose -- plug the hole
            -- right side
            C.inNewAssertionStack $
              do
                St.modify' (`by` add d False)
                rConf >>= St.modify' . wrap
                St.modify' (`by` (&&&) (VariantContext $ bnot $ bRef d))
                (\x -> BBOp op x r) <$> r' >>= evaluate >>= choose
choose (VarCore (BBOp op l (Chc d cl cr))) =
  do let l' = toIL cl
         r' = toIL cr
     sConf <- St.gets sConfig
     -- check if d is in the configuration, and if so what is its value
     T.getModelValue d <$> liftIO (S.sat sConf) >>= \case
       Just True  -> l' >>= choose . VarCore
       Just False -> r' >>= choose . VarCore
       Nothing    -> -- then we have not observed this choice before
         do let sD = C.freshVar d
                lConf = (sConf &&&)        <$> sD -- set Dim to true in conf
                rConf = (sConf &&&) . bnot <$> sD -- Dim is false in conf
            -- left side
            C.inNewAssertionStack $
              do
                St.modify' (`by` add d True)
                lConf >>= St.modify' . wrap -- set conf to new conf with new choice
                St.modify' (`by` (&&&) (VariantContext $ bRef d))
                BBOp op l <$> l' >>= evaluate >>= choose -- plug the hole
            -- right side
            C.inNewAssertionStack $
              do
                St.modify' (`by` add d False)
                rConf >>= St.modify' . wrap
                St.modify' (`by` (&&&) (VariantContext $ bnot $ bRef d))
                BBOp op l <$> r' >>= evaluate >>= choose
-- choose (VarCore (BBOp And l r)) = -- recursive case
--   choose (VarCore l) >> choose (VarCore r)
choose _ = error "it did not cover all the cases"

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
