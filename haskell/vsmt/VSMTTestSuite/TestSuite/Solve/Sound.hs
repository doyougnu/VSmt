-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solve.Sound
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases to ensure the solver is sound up to sbv and consequently smtlib2
-- solvers
-----------------------------------------------------------------------------

module TestSuite.Solve.Sound where

import Data.Text.IO (readFile)
import qualified Data.Text.IO as T

import Data.Parser.Result
import Data.Core.Pretty
import Data.Core.Types

parseGold :: FilePath -> IO ()-- (Prop' Dim, CheckableResult)
parseGold fp = do (Right m) <- parseFromFile fp
                  T.putStrLn $ pretty m
                  return ()
