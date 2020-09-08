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
  , goldenVsStringShow "Choices test" leftChoice'
  ]

allTrue :: IO Result
allTrue = flip sat Nothing $ bRef "foo" &&& bRef "bar" &&& bRef "baz"

leftChoice' :: IO Result
-- leftChoice = flip sat (Just $ VariantContext $ bRef "AA") $ bChc "AA" (bRef "aaa") (bRef "foo")
leftChoice' = flip sat Nothing $ bChc "AA" (bRef "aaa") (bRef "foo") ||| bRef "bar" &&& bRef "baz"

-- leftChoice = show <$> leftChoice'
