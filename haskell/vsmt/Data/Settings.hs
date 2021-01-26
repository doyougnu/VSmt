-----------------------------------------------------------------------------
-- |
-- Module    : Settings
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module for configurations on the solver
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror   #-}
{-# LANGUAGE DeriveGeneric      #-}

module Settings
  ( Settings(..)
  , unSolver
  , defSettings
  , defWithModels
  , defSettingsOnlySat
  , minSettings
  , debugSettings
  , minAsyncSettings
  , with
  , z3
  , yices
  , boolector
  , cvc4
  ) where

import GHC.Generics (Generic)
import qualified Data.SBV  as S (z3,yices,boolector,cvc4, SMTConfig(..), name)

data Settings = Settings { solver           :: Solver
                         , seed             :: Maybe Integer
                         , numResults       :: Maybe Int
                         , generateModels   :: Bool
                         , verboseMode      :: Bool
                         , vcBufSize        :: Int
                         , producerBufSize  :: Int
                         , consumerBufSize  :: Int
                         , numProducers     :: Int
                         , numConsumers     :: Int
                         , numVCWorkers     :: Int
                         } deriving (Show, Generic)

newtype Solver = Solver { unSolver :: S.SMTConfig }
instance Show Solver where show = show . S.name . S.solver . unSolver

with :: Settings -> S.SMTConfig ->  Settings
with s c = s{solver = Solver c}
-- | Convert an interfacial interface to an SMT one
-- | A default configuration uses z3 and tries to shrink propositions
defSettings :: Settings
defSettings = Settings{ solver          = Solver S.z3
                      , seed            = Nothing
                      , generateModels  = False
                      , verboseMode     = False
                      , vcBufSize       = 25000
                      , producerBufSize = 250
                      , consumerBufSize = 250
                      , numResults      = Nothing
                      , numProducers    = 1
                      , numConsumers    = 1
                      , numVCWorkers    = 1
                      }

defSettingsOnlySat :: Settings
defSettingsOnlySat = defSettings{generateModels = False}

minSettings :: Settings
minSettings = Settings{ solver          = Solver S.z3
                      , seed            = Nothing
                      , generateModels  = False
                      , verboseMode     = False
                      , vcBufSize       = 200
                      , producerBufSize = 200
                      , consumerBufSize = 200
                      , numResults      = Nothing
                      , numProducers    = 1
                      , numConsumers    = 1
                      , numVCWorkers    = 1
                      }

minAsyncSettings :: Settings
minAsyncSettings = minSettings{numProducers = 1, numConsumers = 1, numVCWorkers = 1}

debugSettings :: Settings
debugSettings = minSettings{verboseMode=True}

defWithModels :: Settings
defWithModels = defSettings{generateModels=True}

cvc4 :: Settings
cvc4 = defSettings `with` S.cvc4

boolector :: Settings
boolector = defSettings `with` S.boolector

yices :: Settings
yices = defSettings `with` S.yices

z3 :: Settings
z3 = defSettings
