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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- import qualified Data.SBV         as S
import qualified Data.SBV.Control as C
import qualified Data.SBV.Trans   as T
import qualified Data.HashMap.Strict  as Map
import Control.Monad.Except        (MonadError)
import Control.Monad.Reader        (ReaderT)
import qualified Control.Monad.State.Strict as St (MonadState, modify', get)
import Data.Functor.Identity       (Identity)
-- import Control.Monad.Trans         (MonadIO(liftIO), MonadTrans(lift))
import Control.Monad.Trans         (MonadTrans, MonadIO)
import Control.Monad.Logger (MonadLogger)
import qualified Data.Text as Text


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

  add  :: ix -> elem -> Container ix elem -> Container ix elem
  isIn :: ix -> Container ix elem -> Bool
  find :: ix -> Container ix elem -> Maybe elem

instance IxStorable Text.Text where add  = Map.insert
                                    isIn = Map.member
                                    find = Map.lookup

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
data SRef = SI T.SInt32
          | SD T.SDouble
          | SB T.SBool
          deriving Show

data IL = Unit
        | Ref SRef
        | Lit SRef
        | BOp B_B IL
        | BBOp BB_B IL  IL
        | IBOp NN_B IL' IL'
        | Chc Dim Proposition Proposition
        deriving Show

data IL' = Ref' SRef
         | Lit' SRef
         | IOp N_N   IL'
         | IIOp NN_N IL' IL'
         | Chc' Dim NExpression NExpression
         deriving Show

-- TODO: factor out the redundant cases into a type class
toIL :: (T.MonadSymbolic m, St.MonadState s m
        , Has s Bools
        , Has s Ints
        , Has s Doubles
        ) => Proposition -> m IL
toIL (LitB True)  = return $! Lit $ SB T.sTrue
toIL (LitB False) = return $! Lit $ SB T.sFalse
toIL (RefB ref)   = do st <- St.get
                       case find ref $ extract st of
                         Just x  -> return $! Ref $ SB x
                         Nothing -> do newSym <- T.sBool (show ref)
                                       St.modify' (`by` add ref newSym)
                                       return $! Ref $ SB newSym
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
toIL' (LitI (I i)) = Lit' . SI <$> T.sInt32 (show i)
toIL' (LitI (D d)) = Lit' . SD <$> T.sDouble (show d)
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
