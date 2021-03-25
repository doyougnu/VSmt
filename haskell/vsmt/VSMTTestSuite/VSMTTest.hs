-----------------------------------------------------------------------------
-- |
-- Module    : VSMTTest
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Main entry point to the test suite
-----------------------------------------------------------------------------

module Main where

import Test.Tasty
import System.Environment (setEnv)

import qualified TestSuite.Solve.Boolean.Plain
import qualified TestSuite.Solve.Boolean.Choices
import qualified TestSuite.Solve.Arithmetic.Plain
import qualified TestSuite.Solve.Arithmetic.Choices
-- import qualified TestSuite.Solve.Sound
import qualified TestSuite.Solve.Laws.Boolean
import qualified TestSuite.Solve.Laws.Arithmetic
-- import qualified TestSuite.Core.Parser.Result

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "30"
  defaultMain $ testGroup "Tests"
    [ booleans
    , arithmetics
    -- , booleanPs
    -- , arithmeticPs
    -- , parsers
    -- , soundness
      -- laws
    ]

booleans :: TestTree
booleans = testGroup "Boolean Tests"
  [ TestSuite.Solve.Boolean.Plain.tests
  , TestSuite.Solve.Boolean.Choices.tests
  ]

arithmetics :: TestTree
arithmetics = testGroup "Arithmetic Tests"
  [ TestSuite.Solve.Arithmetic.Plain.tests
  , TestSuite.Solve.Arithmetic.Choices.tests
  ]

-- booleanPs :: TestTree
-- booleanPs = testGroup "Boolean Properties"
--   [ TestSuite.Solve.Boolean.Plain.properties
--   ]

-- arithmeticPs :: TestTree
-- arithmeticPs = testGroup "Arithmetic Properties"
--   [ TestSuite.Solve.Arithmetic.Plain.properties
--   ]

-- soundness :: TestTree
-- soundness = testGroup "Soundness of the Solver"
--   [ TestSuite.Solve.Sound.properties
--   ]

laws :: TestTree
laws = testGroup "Laws"
  [ TestSuite.Solve.Laws.Boolean.properties
  , TestSuite.Solve.Laws.Arithmetic.properties
  ]

-- parsers :: TestTree
-- parsers = testGroup "Parser Tests"
--   [
--     TestSuite.Core.Parser.Result.tests
--   ]
