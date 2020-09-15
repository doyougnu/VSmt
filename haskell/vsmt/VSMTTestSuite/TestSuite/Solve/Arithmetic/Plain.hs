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
  [ goldenVsStringShow "simple_arithmetic_to_variable" singletonVar
  , goldenVsStringShow "two_variables_in_addition" twoVars
  ]

singletonVar :: IO Result
singletonVar = flip sat Nothing $ 5 + 2 .== iRef ("y" :: Text)

twoVars :: IO Result
twoVars = flip sat Nothing $ iRef "x" + 2 .== iRef ("y" :: Text)
