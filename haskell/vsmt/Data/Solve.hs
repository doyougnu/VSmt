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

import qualified Data.SBV         as S
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

class IxStorable elem where
  -- type Elem store

  add  :: Text.Text -> elem -> Store Text.Text elem -> Store Text.Text elem
  -- isIn :: ix -> IxMap elem ix -> Bool
  -- find :: ix -> IxMap elem ix -> Maybe elem

-- | this has that
class Has this that where
  extract :: this -> that
  wrap    :: that -> this

  on      :: (that -> that) -> this -> this
  on f = wrap . f . extract

instance IxStorable Bool where
  add  = Map.insert
  -- isIn e (IxMapBool m) = Map.member e m
  -- find k (IxMapBool m) = Map.lookup k m


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

-- avoid lens, generic-deriving dependencies
instance Has State SolverConfig where extract s = config s
                                      wrap    c = mempty{config = c}

instance Has State Ints where extract s = ints s
                              wrap    i = mempty{ints = i}

instance Has State Doubles where extract s = doubles s
                                 wrap    d = mempty{doubles = d}

instance Has State Bools where extract s = bools s
                               wrap    b = mempty{bools = b}


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

data IL = Unit
        | Ref SRef
        | Lit SRef
        | BOp B_B IL
        | BBOp BB_B IL  IL
        | IBOP NN_B IL' IL'
        | Chc Dim Proposition Proposition

data IL' = Ref' SRef
         | Lit' SRef
         | IOp N_N   IL'
         | IIOp NN_N IL' IL'
         | Chc' Dim NExpression NExpression

toIL :: (T.MonadSymbolic m, St.MonadState s m, Has s Bools) => Proposition -> m IL
-- toIL :: Proposition -> Solver IL
toIL (LitB True)  = return $! Lit $ SB S.sTrue
toIL (LitB False) = return $! Lit $ SB S.sFalse
toIL (RefB ref)   = do st <- St.get
                       case Map.lookup ref $ extract st of
                         Just x  -> x
                         Nothing -> do newSym <- T.sBool (show ref)
                                       St.modify' (on $ Map.insert ref newSym)
                                       return $! Ref $ SB newSym
