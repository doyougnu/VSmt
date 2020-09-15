-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solver.Arithmetic.Choices
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases over variational terms for the variational solver
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}

module TestSuite.Solve.Arithmetic.Choices where

import Data.Solve
import Utils.VSMTTestFramework

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Variational formulas"
  [ goldenVsStringShow "Singleton_Choice" singletonChoice
  ]

singletonChoice :: IO Result
singletonChoice = flip sat Nothing $ iChc "AA" (iRef ("Aleft" :: Text)) (iRef "Aright") + 10 .== 23
