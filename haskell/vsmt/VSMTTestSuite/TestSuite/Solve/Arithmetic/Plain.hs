-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solver.Arithmetic.Plain
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases over plain arithmetic terms for the variational solver
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}

module TestSuite.Solve.Arithmetic.Plain where

import Data.Solve
import Data.Text
import Utils.VSMTTestFramework

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Plain formulas"
  [ goldenVsStringShow "simple_arithmetic_to_core" allTrue
  ]

allTrue :: IO Result
allTrue = flip sat Nothing $ (iLit 5 + iLit 2) .== iRef ("y" :: Text)
