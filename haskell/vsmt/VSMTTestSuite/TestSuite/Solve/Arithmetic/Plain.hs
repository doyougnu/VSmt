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

import Data.Text
import Utils.VSMTTestFramework
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM

import Solve
------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Plain formulas"
  [ goldenVsStringShow "simple_arithmetic_to_variable" singletonVar
  , goldenVsStringShow "two_variables_in_addition" twoVars
  , testCase "quickcheck-replay=739384" sub
  , goldenVsStringShow "infinite prop" infinite
    -- goldenVsStringShow "edgeCase" edgeCase
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ plainBecomesUnit
  ]

singletonVar :: IO Result
singletonVar = flip satVerbose Nothing $ 5 + 2 .== iRef ("y" :: Text)

twoVars :: IO Result
twoVars = flip satVerbose Nothing $ iRef "x" + 2 .== iRef ("y" :: Text)

-- | TODO encode numeric equivalences
sub :: Assertion
sub = do
  (core, _) <- solveForCoreVerbose p Nothing
  assertBool "quickcheck says fail" . isUnit $ core
  where p = OpBB Or (OpBB Eqv (RefB "yn") (OpB Not (RefB "mbfd"))) (OpBB Eqv (RefB "d") (OpIB NEQ (RefI (ExRefTypeD "omo")) (RefI (ExRefTypeI "ru"))))

-- | TODO: https://github.com/doyougnu/VSmt/issues/4
plainBecomesUnit :: TestTree
plainBecomesUnit = QC.testProperty
  "For all plain formulas the found variational core is Unit"
  $ \p -> not (isVariational p) QC.==> QCM.monadicIO $
          do (core, _) <- liftIO $ solveForCoreVerbose p Nothing
             QCM.assert $ isUnit core

infinite :: IO Result
infinite = flip satVerbose Nothing p
  where p = iChc "DD" (14.451303985118729 - (-(-22))) (signum 29.38464737058025) .== abs (iChc "EE" (signum (iRef ("uqqhmomillqezrrtwiunpyrdxqy" :: Text))) (-32) + (-29))

-- | fails because there are no variables!
edgeCase :: IO Result
edgeCase = flip satVerbose Nothing p
  where
    p =
      OpIB
        NEQ
        (OpI Sign (LitI (D (-1.504250655492034))))
        (OpI
           Neg
           (OpI
              Sign
              (ChcI
                 "OHGQLFWBZTJFMGPMHAGGUOFYLOQTU"
                 (OpII Div (LitI (I (-52))) (LitI (I (-55))))
                 (LitI (D (-58.00783156897094))))))
