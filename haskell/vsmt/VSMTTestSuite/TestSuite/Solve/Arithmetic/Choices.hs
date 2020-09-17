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
  [ goldenVsStringShow "Singleton_Choice_LHS" singletonChoiceLHS
  , goldenVsStringShow "Singleton_Choice_RHS" singletonChoiceRHS
  , goldenVsStringShow "TwoChoices_LHS" twoChoicesLHS
  , goldenVsStringShow "TwoChoices_RHS" twoChoicesRHS
  , goldenVsStringShow "DeepChoices_LHS" deepChoicesLHS
  ]

singletonChoiceLHS :: IO Result
singletonChoiceLHS = flip satVerbose Nothing $
  iChc "AA" (iRef ("Aleft" :: Text)) (iRef "Aright") + 10 .== 23


singletonChoiceRHS :: IO Result
singletonChoiceRHS = flip satVerbose Nothing $ 100 + iChc "AA" (iRef ("Aleft" :: Text)) (iRef "Aright") .== 23

twoChoicesLHS :: IO Result
twoChoicesLHS = flip satVerbose Nothing $
  (iChc "AA" (iRef ("Aleft" :: Text)) (iRef "Aright") +
  iChc "BB" (iRef "Bleft") (iRef "BRight")) .== 23

twoChoicesRHS :: IO Result
twoChoicesRHS = flip satVerbose Nothing $
  23 .== (iChc "AA" (iRef ("Aleft" :: Text)) (iRef "Aright") +
          iChc "BB" (iRef "Bleft") (iRef "BRight"))

deepChoicesLHS :: IO Result
deepChoicesLHS = flip satVerbose Nothing $
   (1 + 2 + (3 + c)) .== 23
  where c = iChc "AA" (iRef ("Aleft" :: Text)) (iRef "Aright") +
            iChc "BB" (iRef "Bleft") (iRef "BRight")
