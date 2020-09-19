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
import Prelude hiding (EQ, LT, GT)

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Variational formulas"
  [ goldenVsStringShow "Singleton_Choice_LHS" singletonChoiceLHS
  , goldenVsStringShow "Singleton_Choice_RHS" singletonChoiceRHS
  , goldenVsStringShow "TwoChoices_LHS" twoChoicesLHS
  , goldenVsStringShow "TwoChoices_RHS" twoChoicesRHS
  , goldenVsStringShow "DeepChoices_LHS" deepChoicesLHS
  , goldenVsStringShow "MemoryBlowUp" memoryBlow
  , goldenVsStringShow "infinite2" infinite2
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
   (1 + iRef "x" + (3 + c)) .== (23 + iRef "y")
  where c = iChc "AA" (iRef ("Aleft" :: Text)) (iRef "Aright") +
            iChc "BB" (iRef "Bleft") (iRef "BRight")

memoryBlow :: IO Result
memoryBlow = satVerbose p Nothing
  where p = OpBB Or (OpIB EQ (OpII Add (ChcI "RZD" (OpI Sign (RefI (ExRefTypeI "ngyithekwjlolcawjwhwgrgwhwwfvdbuuvirpewkcydekyriasvmwapkx"))) (OpI Abs (RefI (ExRefTypeI "bgvopjtokkmmgbkgugutmouubjf")))) (LitI (I (-48)))) (LitI (I (-40)))) (LitB False)


infinite2 :: IO Result
infinite2 = satVerbose p Nothing
  -- where p = OpB Not (OpIB LTE (ChcI "BQ" (iRef  ("rzoql" :: Text)) (iRef "iyfa")) (LitI (D (-3.835702833833845))))
  where p = OpB Not (OpIB LTE (ChcI "BQ" (iRef  ("rzoql" :: Text)) (iRef "iyfa")) (LitI (D (-3.835702833833845))))
