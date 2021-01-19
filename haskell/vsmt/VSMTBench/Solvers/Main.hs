{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Gauge

import           Bench.Core
import           Utils

main = defaultMain
  [ bgroup "sbv-no-incremental" [ bench "1"    $ nfIO $ sbvSat 1
                                , bench "10"   $ nfIO $ sbvSat 10
                                , bench "100"  $ nfIO $ sbvSat 100
                                , bench "1000" $ nfIO $ sbvSat 1000
                                ]
  , bgroup "sbv-incremental"    [ bench "1"    $ nfIO $ sbvSatInc 1
                                , bench "10"   $ nfIO $ sbvSatInc 10
                                , bench "100"  $ nfIO $ sbvSatInc 100
                                , bench "1000" $ nfIO $ sbvSatInc 1000
                                ]
  , bgroup "z3-no-incremental" [ bench "1"    $ nfIO $ z3Sat 1
                               , bench "10"   $ nfIO $ z3Sat 10
                               , bench "100"  $ nfIO $ z3Sat 100
                               , bench "1000" $ nfIO $ z3Sat 1000
                               ]
  , bgroup "z3-incremental"    [ bench "1"    $ nfIO $ z3SatInc 1
                               , bench "10"   $ nfIO $ z3SatInc 10
                               , bench "100"  $ nfIO $ z3SatInc 100
                               , bench "1000" $ nfIO $ z3SatInc 1000
                               ]
  ]
