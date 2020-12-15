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
                         , readerBufSize    :: Int
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
                      , verboseMode     = True
                      , vcBufSize       = 250
                      , producerBufSize = 250
                      , readerBufSize   = 250
                      , numResults      = Nothing
                      , numProducers    = 250
                      , numConsumers    = 50
                      , numVCWorkers    = 10
                      }

defSettingsOnlySat :: Settings
defSettingsOnlySat = defSettings{generateModels = False}

defSettingsNoVerbose :: Settings
defSettingsNoVerbose = defSettings{solver=Solver $ S.z3{S.verbose=False}}

minSettings :: Settings
minSettings = Settings{ solver          = Solver S.z3
                      , seed            = Nothing
                      , generateModels  = False
                      , verboseMode     = True
                      , vcBufSize       = 1
                      , producerBufSize = 1
                      , readerBufSize   = 1
                      , numResults      = Nothing
                      , numProducers    = 1
                      , numConsumers    = 1
                      , numVCWorkers    = 1
                      }

debugSettings :: Settings
debugSettings = minSettings{solver = Solver S.z3{S.verbose=True}, verboseMode=True}

debugSettingsNoVerbose :: Settings
debugSettingsNoVerbose = minSettings{verboseMode=True}
