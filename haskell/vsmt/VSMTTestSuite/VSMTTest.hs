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

{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Test.Tasty

import qualified TestSuite.Solve.Boolean.Plain
import qualified TestSuite.Solve.Boolean.Choices
import qualified TestSuite.Solve.Arithmetic.Plain
import qualified TestSuite.Solve.Arithmetic.Choices

main :: IO ()
main = defaultMain $
  testGroup "Local"
  [ -- booleans
  arithmetics
  ]

booleans :: TestTree
booleans = testGroup "Boolean Tests"
  [ TestSuite.Solve.Boolean.Plain.tests
  , TestSuite.Solve.Boolean.Choices.tests
  , TestSuite.Solve.Arithmetic.Plain.tests
  ]

arithmetics :: TestTree
arithmetics = testGroup "Arithmetic Tests"
  [ TestSuite.Solve.Arithmetic.Plain.tests
  , TestSuite.Solve.Arithmetic.Choices.tests
  ]
