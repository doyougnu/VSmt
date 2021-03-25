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
  , defSettings
  , defWithModels
  , defSettingsOnlySat
  , minSettings
  , debugSettings
  , minAsyncSettings
  ) where

import GHC.Generics (Generic)

data Settings = Settings { seed             :: Maybe Integer
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


-- | Convert an interfacial interface to an SMT one
-- | A default configuration uses z3 and tries to shrink propositions
defSettings :: Settings
defSettings = Settings{ seed            = Nothing
                      , generateModels  = True
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
minSettings = Settings{ seed            = Nothing
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
