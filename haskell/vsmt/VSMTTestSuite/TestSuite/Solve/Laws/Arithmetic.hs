-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solver.Laws.Arithmetic
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Property based tests over mathematics laws which should always hold
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

module TestSuite.Solve.Laws.Arithmetic where

import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM

import Solve
import Utils.VSMTTestFramework

properties :: TestTree
properties =
  testGroup
    "Arithmetic" [ QC.testProperty "addition is commutative"       addCommutativity
                 , QC.testProperty "multiplication is commutative" multCommutativity
                 ]

------------------------------ Boolean Laws ------------------------------------
-- | shorthand type synonyms for properties over binary relations
type UnaryProperty  = OnlyArith -> QC.Property

-- | shorthand type synonyms for properties over binary relations
type BinaryProperty  = OnlyArith -> OnlyArith -> QC.Property

-- | shorthand type synonyms for properties over ternary relations
type TernaryProperty = OnlyArith -> OnlyArith -> OnlyArith -> QC.Property

---------------------------- Special Cases -------------------------------------
-- andAnnhilates :: UnaryProperty
-- andAnnhilates (unOnlyBools -> p) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings (p &&& false)
--      right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings false
--      return $! left == right

-- orAnnhilates :: UnaryProperty
-- orAnnhilates (unOnlyBools -> p) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defSettings (p ||| true)
--      right <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defSettings true
--      -- note that we need to check the unsat count because the sat cnt could
--      -- change depending on if we have a choice:
--      -- chc D a b ||| true
--      -- could have 2 models but
--      -- true, only has one
--      -- in this case annilation is still preserved
--      return $! left == right

-- andIdentity :: UnaryProperty
-- andIdentity (unOnlyBools -> p) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings (p &&& true)
--      right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings p
--      return $! left == right

-- orIdentity :: UnaryProperty
-- orIdentity (unOnlyBools -> p) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings (p ||| false)
--      right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings p
--      return $! left == right

-- andOverOr :: TernaryProperty
-- andOverOr (unOnlyBools -> p) (unOnlyBools -> q) (unOnlyBools -> r) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings (p &&& (q ||| r))
--      right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings ((p &&& q) ||| (p &&& r))
--      return $! left == right

-- orOverAnd :: TernaryProperty
-- orOverAnd (unOnlyBools -> p) (unOnlyBools -> q) (unOnlyBools -> r) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings (p ||| (q &&& r))
--      right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings ((p ||| q) &&& (p ||| r))
--      return $! left == right


-- andAbsorbsOr :: BinaryProperty
-- andAbsorbsOr (unOnlyBools -> p) (unOnlyBools -> q) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defSettings (p &&& (p ||| q))
--      right <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defSettings p
--      -- same problem as testing annhilation
--      return $! left == right

-- orAbsorbsAnd :: BinaryProperty
-- orAbsorbsAnd (unOnlyBools -> p) (unOnlyBools -> q) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defSettings (p ||| (p &&& q))
--      right <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defSettings p
--      -- same problem as testing annhilation
--      return $! left == right

-- ---------------------------- General Cases -------------------------------------
-- doubleNegation :: UnaryProperty
-- doubleNegation (unOnlyBools -> p) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings p
--      right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings (bnot $ bnot p)
--      return $! left == right

commutes :: (NExpression -> NExpression -> NExpression) -> BinaryProperty
commutes operator (unOnlyArith -> p) (unOnlyArith -> q) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (0 .== (p `operator` q))
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (0 .== (q `operator` p))
     return $! left == right

-- associates :: (Proposition -> Proposition -> Proposition) -> TernaryProperty
-- associates operator (unOnlyBools -> p) (unOnlyBools -> q) (unOnlyBools -> r) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings ((p `operator` q) `operator` r)
--      right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings (p `operator` (q `operator` r))
--      return $! left == right

-- idempotence :: (Proposition -> Proposition -> Proposition) -> UnaryProperty
-- idempotence operator (unOnlyBools -> p) = QCM.monadicIO $
--   do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings (p `operator` p)
--      right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings p
--      return $! left == right


addCommutativity :: BinaryProperty
addCommutativity = commutes (+)

multCommutativity :: BinaryProperty
multCommutativity = commutes (*)
