-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solver.Boolean.Plain
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases over plain terms for the variational solver
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}

module TestSuite.Solve.Boolean.Plain where

import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM

import Utils.VSMTTestFramework

import Solve
------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Plain formulas"
  [ goldenVsStringShow "conjunctions_force_True" allTrue
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ plainBecomesUnit
  ]

-- | TODO encode known boolean equivalences
allTrue :: IO Result
allTrue = solve Nothing defSettings $ bRef "foo" &&& bRef "bar" &&& bRef "baz"

plainBecomesUnit :: TestTree
plainBecomesUnit = QC.testProperty
  "For all plain formulas the found variational core is Unit"
  $ \p -> not (isVariational p) QC.==> QCM.monadicIO $
          do core <- liftIO $ sFst <$> solveForCore p
             QCM.assert $ isUnit core
