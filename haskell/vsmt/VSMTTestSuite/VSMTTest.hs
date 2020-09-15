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

import qualified TestSuite.Solve.Plain
import qualified TestSuite.Solve.Choices

main :: IO ()
main = defaultMain $
  testGroup "Local"
  [ localOnlyTest

  ]

localOnlyTest :: TestTree
localOnlyTest = testGroup "VSMTLocalOnlyTests"
  [ TestSuite.Solve.Plain.tests
  , TestSuite.Solve.Choices.tests
  ]
