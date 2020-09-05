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

module Data.Solve where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.Logger       (LoggingT, MonadLogger (..),
                                             logDebug, runStdoutLoggingT)
import qualified Control.Monad.State.Strict as St (MonadState, StateT, get, gets
                                                   , modify', runStateT)
import           Control.Monad.Trans        (MonadIO, MonadTrans, lift)
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe                 (fromJust)
import qualified Data.SBV                   as S
import           Data.SBV.Internals         (SolverContext (..))
import qualified Data.SBV.Trans             as T
import qualified Data.SBV.Trans.Control     as C
import qualified Data.Text                  as Text
import           Prelude                    hiding (EQ, GT, LT, log)

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

------------------------------ Internal Api -------------------------------------
findVCore :: IL -> Solver VarCore
findVCore = evaluate

solution :: (St.MonadState State m, Has Result) => m Result
solution = extract <$> St.get

-- | TODO abstract over the logging function
solve :: Proposition -> IO (Result, State)
solve i = T.runSMT $
  do (il, st) <- runStdoutLoggingT $ St.runStateT (toIL i) mempty
     C.query $ runStdoutLoggingT $ runSolver st $
       do core <- findVCore il
          logWith "Core" core
          _ <- choose core
          solution

sat :: Proposition -> IO Result
sat = fmap fst <$> solve

------------------------------ Data Types --------------------------------------
-- | Solver configuration is a mapping of dimensions to boolean values, we must
-- use an actual data structure here because we need set-like operations
type Store = Map.HashMap

type Ints         = Store Var T.SInt32
type Doubles      = Store Var T.SDouble
type Bools        = Store Var T.SBool

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

instance IxStorable Text.Text where add  = Map.insert
                                    isIn = Map.member
                                    find = Map.lookup
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
instance Has VariantContext where
  type Contains VariantContext = State
  extract   = config
  wrap    c w = w{config = c}

instance Has Ints    where extract   = ints
                           wrap    i w = w{ints = i}

instance Has Doubles where extract   = doubles
                           wrap    d w = w{doubles = d}

instance Has Bools   where extract   = bools
                           wrap    b w = w{bools = b}

instance Has Result  where extract   = result
                           wrap    r w = w{result=r}

-- | The internal state of the solver is just a record that accumulates results
-- and a configuration to track choice decisions
data State = State
    { result  :: Result
    , config  :: VariantContext
    , ints    :: Ints
    , doubles :: Doubles
    , bools   :: Bools
    }

instance Semigroup State where
  a <> b = State { result  = result  a <> result  b
                 , config  = config  a <> config  b
                 , ints    = ints    a <> ints    b
                 , doubles = doubles a <> doubles b
                 , bools   = bools   a <> bools   b
                 }

instance Monoid State where
  mempty = State{ result  = mempty
                , config  = mempty
                , ints    = mempty
                , doubles = mempty
                , bools   = mempty
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

-- TODO use standalone deriving + deriving via
instance MonadLogger m     => MonadLogger (C.QueryT m)     where monadLoggerLog = monadLoggerLog
instance MonadLogger m     => MonadLogger (T.SymbolicT m)  where monadLoggerLog = monadLoggerLog
instance C.MonadQuery m    => C.MonadQuery (LoggingT m)    where queryState     = lift C.queryState
instance T.MonadSymbolic m => T.MonadSymbolic (LoggingT m) where symbolicEnv    = lift T.symbolicEnv

-- | A solver type enabled with query operations and logging
type Solver = SolverT (LoggingT C.Query)

runSolver :: State -> SolverT m a -> m (a, State)
runSolver s = flip St.runStateT s . runSolverT

-- TODO change to log to a log file
-- TODO run function which does no logging
run :: State -> Solver a -> IO (a, State)
run s = S.runSMTWith S.z3{S.verbose=True} . C.query . runStdoutLoggingT . runSolver s

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
toIL :: (T.MonadSymbolic m, St.MonadState State m , MonadLogger m)
     => Proposition -> m IL
toIL (LitB True)  = return $! Ref T.sTrue
toIL (LitB False) = return $! Ref T.sFalse
toIL (RefB ref)   = do st <- St.get
                       case find ref $ extract st of
                         Just x  -> return $! Ref x
                         Nothing -> do newSym <- T.sBool (show ref)
                                       St.modify' (`by` add ref newSym)
                                       return $! Ref newSym
toIL (OpB op e)  = BOp op <$> toIL e
toIL (OpBB op l r) = do l' <- toIL l; r' <- toIL r; return $ BBOp op l' r'
toIL (OpIB op l r) = do l' <- toIL' l; r' <- toIL' r; return $ IBOp op l' r'
toIL (ChcB d l r)  = return $ Chc d l r

toIL' :: (T.MonadSymbolic m, St.MonadState State m
         , Has Ints, Has Doubles) => NExpression -> m IL'
toIL' (LitI (I i)) = Ref' . SI <$> T.sInt32 (show i)
toIL' (LitI (D d)) = Ref' . SD <$> T.sDouble (show d)
toIL' (RefI ExRefTypeI a) = do st <- St.get
                               case find a $ extract st of
                                 Just x  -> return $! Ref' $ SI x
                                 Nothing -> do newSym <- T.sInt32 (show a)
                                               St.modify' (`by` add a newSym)
                                               return $! Ref' $ SI newSym
toIL' (RefI ExRefTypeD a) = do st <- St.get
                               case find a $ extract st of
                                 Just x  -> return $! Ref' $ SD x
                                 Nothing -> do newSym <- T.sDouble (show a)
                                               St.modify' (`by` add a newSym)
                                               return $! Ref' $ SD newSym
toIL' (OpI op e)                = IOp op <$> toIL' e
toIL' (OpII op l r)    = do l' <- toIL' l; r' <- toIL' r; return $! IIOp op l' r'
toIL' (ChcI d l r) = return $ Chc' d l r

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
evaluate :: (St.MonadState State m, Monad m, MonadLogger m, SolverContext m)
  => IL -> m VarCore
  -- computation rules
evaluate Unit     = return $! intoCore Unit
evaluate (Ref b)  = do logWith "got reference" b
                       bs <- bools <$> St.get
                       logWith "bools" bs >> toSolver b
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
evaluate (BBOp And l x@(Ref _)) = evaluate x >> evaluate l
evaluate (BBOp And x@(Ref _) r) = evaluate x >> evaluate r
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
store :: ( St.MonadState (Contains that) m
         , Has that
         , Semigroup that) => that -> m ()
store = St.modify' . flip by . (<>)

-- choose :: ( Has Result
--           , Has VariantContext
--           , St.MonadState State m
--           , MonadIO m
--           , T.MonadSymbolic m
--           , Monad m
--           , MonadLogger m
--           , SolverContext m
--           , C.MonadQuery m) => VarCore -> m ()
choose :: VarCore -> Solver ()
choose (VarCore Unit) = St.get >>= getResult . extract >>= store

choose (VarCore (Chc d l r)) = do conf <- St.gets extract
                                  let l' = toIL l -- keep these lazy they may
                                      r' = toIL r -- not be chosen
                                  case T.sat conf >>= T.getModelValue (Text.unpack d) of
                                    Just True -> toIL l >>= choose . VarCore
                                    Just False -> toIL r >>= choose . VarCore
                                    Nothing -> do let lConf = conf &&& (VariantContext $! bRef d)
                                                      rConf = conf &&& (VariantContext $! bnot $ bRef d)
                                                  -- left side
                                                  C.inNewAssertionStack $
                                                    do St.modify' (`by` const lConf)
                                                       l' >>= evaluate >>= choose
                                                  -- right side
                                                  C.inNewAssertionStack $
                                                    do St.modify' (flip by (const rConf))
                                                       r' >>= evaluate >>= choose
