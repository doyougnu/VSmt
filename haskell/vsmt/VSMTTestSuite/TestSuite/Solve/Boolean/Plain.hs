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

import Data.Solve
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import Utils.VSMTTestFramework

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Plain formulas"
  [ goldenVsStringShow "conjunctions_force_True" allTrue
  ]

properties :: TestTree
properties = testGroup "Properties"
  []

allTrue :: IO Result
allTrue = flip sat Nothing $ bRef "foo" &&& bRef "bar" &&& bRef "baz"

plain_evals_to_unit :: Proposition -> QC.Property
plain_evals_to_unit p = isPlain p QC.==> QCM.monadicIO $
                        do return ()
