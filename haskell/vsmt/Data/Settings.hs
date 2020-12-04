-----------------------------------------------------------------------------
-- |
-- Module    : Data.Settings
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module for configurations on the solver
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror   #-}
{-# LANGUAGE DeriveGeneric      #-}

module Data.Settings
  ( Settings(..)
  , getConfig
  , defSettings
  , defSettingsOnlySat
  , minSettings
  , debugSettings
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
                      , verboseMode     = False
                      , vcBufSize       = 250
                      , producerBufSize = 250
                      , readerBufSize   = 250
                      , numResults      = Nothing
                      , numProducers    = 100
                      , numReaders      = 50
                      }

defSettingsOnlySat :: Settings
defSettingsOnlySat = defSettings{generateModels = False}

minSettings :: Settings
minSettings = Settings{ solver          = Solver S.z3
                      , seed            = Nothing
                      , generateModels  = False
                      , verboseMode     = False
                      , vcBufSize       = 1
                      , producerBufSize = 1
                      , readerBufSize   = 1
                      , numResults      = Nothing
                      , numProducers    = 1
                      , numReaders      = 1
                      }

debugSettings :: Settings
debugSettings = minSettings{verboseMode=True}
