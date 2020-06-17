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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.SBV         as S
import qualified Data.SBV.Control as C
import Control.Monad.Except        (MonadError, ExceptT)
import Control.Monad.Reader        (MonadReader(..), ReaderT, runReaderT,
                                    mapReaderT)
import Control.Monad.State.Lazy    (MonadState)
import Control.Monad.Trans         (MonadIO(liftIO), MonadTrans(lift))
import Control.Monad.Trans.Maybe   (MaybeT)
import Control.Monad.Logger (MonadLogger)

import           Data.Core.Result
import           Data.Core.Types

data State = State (Int,Int)

newtype SolverT m a = SolverT { runSolverT :: ReaderT State m a }
  deriving (Functor,Applicative,Monad,MonadIO -- base
           ,MonadTrans, MonadError e, MonadState s, MonadLogger)
