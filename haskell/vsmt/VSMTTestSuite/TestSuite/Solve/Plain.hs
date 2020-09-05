-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solver.Plain
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases over plain terms for the variational solver
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}

module TestSuite.Solve.Plain where

import Data.Solve
import Utils.VSMTTestFramework

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Plain formulas"
  [ goldenVsStringShow "conjunctions_force_True" allTrue
  ]

allTrue :: IO Result
allTrue = sat $ bRef "foo" &&& bRef "bar" &&& bRef "baz"
