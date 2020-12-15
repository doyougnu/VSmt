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
                         , numReaders       :: Int
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
                      , numReaders      = 50
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
                      , vcBufSize       = 10
                      , producerBufSize = 10
                      , readerBufSize   = 10
                      , numResults      = Nothing
                      , numProducers    = 2
                      , numReaders      = 2
                      }

debugSettings :: Settings
debugSettings = minSettings{verboseMode=True}
