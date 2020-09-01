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
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Solver where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.Reader       (ReaderT)
import qualified Control.Monad.State.Strict as St (MonadState, get, modify')
import           Data.Functor.Identity      (Identity)
import qualified Data.HashMap.Strict        as Map
import qualified Data.SBV                   as S
import qualified Data.SBV.Control           as C
import qualified Data.SBV.Trans             as T
import           Prelude                    hiding (EQ, GT, LT)
-- import Control.Monad.Trans         (MonadIO(liftIO), MonadTrans(lift))
import           Control.Monad.Logger       (MonadLogger)
import           Control.Monad.Trans        (MonadIO, MonadTrans)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as Text


import           Data.Core.Result
import           Data.Core.Types

-- | Solver configuration is a mapping of dimensions to boolean values, we must
-- use an actual data structure here because we need set-like operations
type Store = Map.HashMap

type SolverConfig = Store Dim Bool
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


-- | this has that
class Has this that where
  extract :: this -> that
  wrap    :: that -> this

  by      :: this -> (that -> that) -> this
  by s f = wrap . f . extract $ s

-- avoiding lens, generic-deriving dependencies
instance Has State SolverConfig where extract s = config s
                                      wrap    c = mempty{config = c}

instance Has State Ints where extract s = ints s
                              wrap    i = mempty{ints = i}

instance Has State Doubles where extract s = doubles s
                                 wrap    d = mempty{doubles = d}

instance Has State Bools where extract s = bools s
                               wrap    b = mempty{bools = b}

-- | The internal state of the solver is just a record that accumulates results
-- and a configuration to track choice decisions
data State = State { result  :: Result Var
                   , config  :: SolverConfig
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

newtype SolverT m a = SolverT { runSolverT :: ReaderT State m a }
  deriving ( Functor,Applicative,Monad,MonadIO -- base
           , MonadTrans, MonadError e, St.MonadState s, MonadLogger
           , T.MonadSymbolic, C.MonadQuery
           )

instance C.Fresh (SolverT m) a where fresh = C.fresh

type Solver = SolverT Identity

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
        | BBOp BB_B IL  IL
        | IBOp NN_B IL' IL'
        | Chc Dim Proposition Proposition
        deriving Show

data IL' = Ref' NRef
         | IOp N_N   IL'
         | IIOp NN_N IL' IL'
         | Chc' Dim NExpression NExpression
         deriving Show

-- TODO: factor out the redundant cases into a type class
-- | Convert a proposition into the intermediate language to generate a
-- Variational Core
toIL :: (T.MonadSymbolic m, St.MonadState s m
        , Has s Bools
        , Has s Ints
        , Has s Doubles
        ) => Proposition -> m IL
toIL (LitB True)  = return $! Ref T.sTrue
toIL (LitB False) = return $! Ref T.sFalse
toIL (RefB ref)   = do st <- St.get
                       case find ref $ extract st of
                         Just x  -> return $! Ref x
                         Nothing -> do newSym <- T.sBool (show ref)
                                       St.modify' (`by` add ref newSym)
                                       return $! Ref newSym
toIL (OpB op e)  = BOp op <$> toIL e
toIL (OpBB op l r) = do l' <- toIL l
                        r' <- toIL r
                        return $ BBOp op l' r'
toIL (OpIB op l r) = do l' <- toIL' l
                        r' <- toIL' r
                        return $ IBOp op l' r'
toIL (ChcB d l r)  = return $ Chc d l r

toIL' :: (T.MonadSymbolic m, St.MonadState s m
         , Has s Ints, Has s Doubles) => NExpression -> m IL'
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
toIL' (OpII op l r)    = do l' <- toIL' l
                            r' <- toIL' r
                            return $! IIOp op l' r'
toIL' (ChcI d l r) = return $ Chc' d l r

-------------------------------- Accumulation -----------------------------------
newtype VarCore = VarCore IL

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
