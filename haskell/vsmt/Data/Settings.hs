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
  , getConfig
  , defSettings
  , defSettingsOnlySat
  , minSettings
  , debugSettings
  , defSettingsNoVerbose
  , debugSettingsNoVerbose
  , defSettingsVerbose
  , minAsyncSettings
  , minAsyncDebug
  ) where

import GHC.Generics (Generic)

import qualified Data.SBV  as S (z3, SMTConfig(..), name)

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

newtype Solver = Solver { getConfig :: S.SMTConfig }
instance Show Solver where show = show . S.name . S.solver . getConfig

-- | Convert an interfacial interface to an SMT one
-- | A default configuration uses z3 and tries to shrink propositions
defSettings :: Settings
defSettings = Settings{ solver          = Solver S.z3
                      , seed            = Nothing
                      , generateModels  = True
                      , verboseMode     = False
                      , vcBufSize       = 250
                      , producerBufSize = 250
                      , consumerBufSize = 250
                      , numResults      = Nothing
                      , numProducers    = 1
                      , numConsumers    = 1
                      , numVCWorkers    = 1
                      }

defSettingsOnlySat :: Settings
defSettingsOnlySat = defSettings{generateModels = False}

defSettingsNoVerbose :: Settings
defSettingsNoVerbose = defSettings{solver=Solver $ S.z3{S.verbose=False}}

defSettingsVerbose :: Settings
defSettingsVerbose = defSettings{solver=Solver $ S.z3{S.verbose=True}}

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

minAsyncDebug :: Settings
minAsyncDebug = minSettings{solver = Solver S.z3{S.verbose=True}, numProducers = 1, numConsumers = 1, numVCWorkers = 1, verboseMode=True}

debugSettings :: Settings
debugSettings = minSettings{solver = Solver S.z3{S.verbose=True}, verboseMode=True}

debugSettingsNoVerbose :: Settings
debugSettingsNoVerbose = minSettings{solver = Solver S.z3{S.verbose=False}}
