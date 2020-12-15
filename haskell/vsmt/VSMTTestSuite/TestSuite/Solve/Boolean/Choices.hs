-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solver.Boolean.Choices
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases over variational terms for the variational solver
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}

module TestSuite.Solve.Boolean.Choices where

import Solve
import Utils.VSMTTestFramework

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Variational formulas"
  [ goldenVsStringShow "Singleton_Choice" singletonChoice
  , goldenVsStringShow "Two_Choices" twoChoices
  , goldenVsStringShow "Tree_of_Choices" treeOfChoices
  , goldenVsStringShow "Single_Choice_With_Vars" singletonChoiceWithVars
  , goldenVsStringShow "Single_Choice_With_VC"  singletonChoiceWithVC
  , goldenVsStringShow "Two_Choices_With_VC"  twoChoicesWithVC
  ]

singletonChoice :: IO Result
singletonChoice = flip satVerbose Nothing $ bChc "AA" (bRef "Aleft") (bRef "Aright")

singletonChoiceWithVC :: IO Result
singletonChoiceWithVC = flip satVerbose (Just $ toVariantContext (bRef "AA")) $ bChc "AA" (bRef "Aleft") (bRef "Aright")

singletonChoiceWithVars :: IO Result
singletonChoiceWithVars = flip satVerbose Nothing $ (bChc "AA" (bRef "Aleft") (bRef "Aright") ||| bRef "one") &&& bRef "two"

twoChoices :: IO Result
twoChoices = flip satVerbose Nothing $
             bChc "AA" (bRef "Aleft") (bRef "Aright") &&&  bChc "BB" (bRef "Bleft") (bRef "BRight")

twoChoicesWithVC :: IO Result
twoChoicesWithVC = flip satVerbose (Just $ toVariantContext $ (bRef "AA") &&& (bnot $ bRef "BB")) $
             bChc "AA" (bRef "Aleft") (bRef "Aright") &&&  bChc "BB" (bRef "Bleft") (bRef "BRight")

-- | test to make sure the solver returns true for every variable assignment
-- where the asst will _not_ have a choice as a child of the root node. This
-- forces the solver to rotate the variational core
treeOfChoices :: IO Result
treeOfChoices = flip satVerbose Nothing $
             (bChc "AA" (bRef "Aleft") (bRef "Aright") &&&
             bChc "BB" (bRef "Bleft") (bRef "Bright")) &&&
             (bChc "CC" (bRef "Cleft") (bRef "Cright") &&&
             bChc "DD" (bRef "Dleft") (bRef "Dright"))
