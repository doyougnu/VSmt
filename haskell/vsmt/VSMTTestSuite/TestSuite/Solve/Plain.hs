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
  , goldenVsStringShow "Singleton_Choice" singletonChoice
  , goldenVsStringShow "Two_Choices" twoChoices
  ]

allTrue :: IO Result
allTrue = flip sat Nothing $ bRef "foo" &&& bRef "bar" &&& bRef "baz"

singletonChoice :: IO Result
singletonChoice = flip sat Nothing $ bChc "AA" (bRef "Aleft") (bRef "Aright")

twoChoices :: IO Result
twoChoices = flip sat Nothing $
             bChc "AA" (bRef "Aleft") (bRef "Aright") &&&  bChc "BB" (bRef "Bleft") (bRef "BRight")
