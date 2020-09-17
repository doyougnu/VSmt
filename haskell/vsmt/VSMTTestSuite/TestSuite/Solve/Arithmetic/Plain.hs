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
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Plain formulas"
  [ goldenVsStringShow "simple_arithmetic_to_variable" singletonVar
  , goldenVsStringShow "two_variables_in_addition" twoVars
  , testCase "quickcheck-replay=739384" sub
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
  $ \p -> isPlain p QC.==> QCM.monadicIO $
          do (core, _) <- liftIO $ solveForCoreVerbose p Nothing
             QCM.assert $ isUnit core
