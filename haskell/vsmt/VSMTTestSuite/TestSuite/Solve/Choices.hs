-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solver.Choices
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases over variational terms for the variational solver
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}

module TestSuite.Solve.Choices where

import Data.Solve
import Utils.VSMTTestFramework

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Variational formulas"
  [ goldenVsStringShow "Singleton_Choice" singletonChoice
  , goldenVsStringShow "Two_Choices" twoChoices
  , goldenVsStringShow "Tree_of_Choices" treeOfChoices
  ]

singletonChoice :: IO Result
singletonChoice = flip sat Nothing $ bChc "AA" (bRef "Aleft") (bRef "Aright")

twoChoices :: IO Result
twoChoices = flip sat Nothing $
             bChc "AA" (bRef "Aleft") (bRef "Aright") &&&  bChc "BB" (bRef "Bleft") (bRef "BRight")

-- | test to make sure the solver returns true for every variable assignment
-- where the asst will _not_ have a choice as a child of the root node. This
-- forces the solver to rotate the variational core
treeOfChoices :: IO Result
treeOfChoices = flip sat Nothing $
             bChc "AA" (bRef "Aleft") (bRef "Aright") &&&
             bChc "BB" (bRef "Bleft") (bRef "Bright") &&&
             bChc "CC" (bRef "Cleft") (bRef "Cright") &&&
             bChc "DD" (bRef "Dleft") (bRef "Dright") &&&
             bChc "AAA" (bRef "AAleft") (bRef "AAright") &&&
             bChc "BBB" (bRef "BBleft") (bRef "BBright") &&&
             bChc "CCC" (bRef "CCleft") (bRef "CCright")
