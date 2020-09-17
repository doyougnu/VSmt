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

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "2000"
  defaultMain $ testGroup "Tests"
    [ booleans
    , arithmetics
    , booleanPs
    , arithmeticPs
    ]

booleans :: TestTree
booleans = testGroup "Boolean Tests"
  [ TestSuite.Solve.Boolean.Plain.tests
  , TestSuite.Solve.Boolean.Choices.tests
  , TestSuite.Solve.Boolean.Plain.properties
  ]

arithmetics :: TestTree
arithmetics = testGroup "Arithmetic Tests"
  [ TestSuite.Solve.Arithmetic.Plain.tests
  , TestSuite.Solve.Arithmetic.Choices.tests
  , TestSuite.Solve.Arithmetic.Plain.properties
  ]

booleanPs :: TestTree
booleanPs = testGroup "Boolean Properties"
  [ TestSuite.Solve.Boolean.Plain.properties
  ]

arithmeticPs :: TestTree
arithmeticPs = testGroup "Arithmetic Properties"
  [ TestSuite.Solve.Arithmetic.Plain.properties
  ]
