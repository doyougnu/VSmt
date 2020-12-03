-----------------------------------------------------------------------------
-- |
-- Module    : Data.Config
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module for configurations on the solver
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Config where

import Data.SBV.Control (SMTOption(..))
import Data.SBV (SMTConfig(..),z3,yices,mathSAT,boolector,abc,cvc4)
import GHC.Generics (Generic)
import Data.Foldable (foldr')

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
                         } deriving (Show,Generic)

-- | TODO add field for ops
data SMTConf = SMTConf { conf :: !SMTConfig
                       , settings :: Settings
                       }

data Solver = Z3
            | Yices
            | MathSat
            | Boolector
            | Abc
            | Cvc4
            deriving (Show,Generic)

-- applyOpts :: SMTConf -> Proposition -> Proposition
-- applyOpts conf p = foldr' ($) p (opts conf)

-- | Convert an interfacial interface to an SMT one
toConf :: Settings -> SMTConf
toConf Settings{..} = foldr' ($) emptyConf ss
  where ss = [setSeed seed, setSolver solver, setModelGen generateModels]

-- | A default configuration uses z3 and tries to shrink propositions
defSettings :: Settings
defSettings = Settings{ solver          = Z3
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

-- moveRight required for proper results
minSettings :: Settings
minSettings = Settings{ solver          = Z3
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

defConf :: SMTConf
defConf = toConf defSettings

defConfOnlySat :: SMTConf
defConfOnlySat = toConf defSettingsOnlySat

emptyConf :: SMTConf
emptyConf = SMTConf{conf=z3, settings=defSettings}

debugConf :: SMTConf
debugConf = setVerbose $ toConf debugSettings

minConf :: SMTConf
minConf = toConf minSettings

-- | apply some function on the solver options. This could be done more cleanly
-- with lenses but I don't want to bloat the library
addOption :: ([SMTOption] -> [SMTOption]) -> SMTConf -> SMTConf
addOption f c = SMTConf { conf = c'{solverSetOptions=f sOpts}
                        , settings = settings c
                        }
  where c' = conf c
        sOpts = solverSetOptions c'

-- | set the seed of the internal solver
setSeed :: Maybe Integer -> SMTConf -> SMTConf
setSeed (Just x) c = addOption (RandomSeed x:) c
setSeed Nothing  c = c


setVerbose :: SMTConf -> SMTConf
setVerbose SMTConf{..} = SMTConf{conf=conf{verbose=True},settings}

setSolver :: Solver -> SMTConf -> SMTConf
setSolver Z3 a        = a{conf=z3}
setSolver Yices a     = a{conf=yices}
setSolver MathSat a   = a{conf=mathSAT}
setSolver Boolector a = a{conf=boolector}
setSolver Abc a       = a{conf=abc}
setSolver Cvc4 a      = a{conf=cvc4}

-- setOpts :: [Opts] -> SMTConf -> SMTConf
-- setOpts os c = c{opts=convertOpts <$> os}

setModelGen :: Bool -> SMTConf -> SMTConf
setModelGen b SMTConf{..} = SMTConf{ conf
                                   ,settings=settings{generateModels = b}}

z3DefConf ::  SMTConf
z3DefConf = setSolver Z3 defConf

z3DefConfOnlySat :: SMTConf
z3DefConfOnlySat = setSolver Z3 defConfOnlySat

yicesDefConf :: SMTConf
yicesDefConf = setSolver Yices defConf

mathSatDefConf :: SMTConf
mathSatDefConf = setSolver MathSat defConf

boolectorDefConf :: SMTConf
boolectorDefConf = setSolver Boolector defConf

cvc4DefConf :: SMTConf
cvc4DefConf = setSolver Cvc4 defConf

abcDefConf :: SMTConf
abcDefConf = setSolver Abc defConf
