-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solver.Laws.Boolean
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Property based tests over logical laws which should always hold
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module TestSuite.Solve.Laws.Boolean where

import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM

import Solve
import Utils.VSMTTestFramework

properties :: TestTree
properties =
  testGroup
    "Booleans" [ QC.testProperty "double negation is no negation" doubleNegation
               , QC.testProperty "and is commutative"             andCommutativity
               , QC.testProperty "or is commutative"              orCommutativity
               , QC.testProperty "equivalence is commutative"     eqCommutativity
               , QC.testProperty "xor is commutative"             xorCommutativity

               , QC.testProperty "and is associative"             andAssociativity
               , QC.testProperty "or is associative"              orAssociativity
               , QC.testProperty "equivalence is associative"     eqAssociativity
               , QC.testProperty "xor is associative"             xorAssociativity

               , QC.testProperty "and annihilates"                andAnnhilates
               , QC.testProperty "or annihilates"                 orAnnhilates

               , QC.testProperty "and identity"                   andIdentity
               , QC.testProperty "or identity"                    orIdentity

               , QC.testProperty "and idempotent"                 andIdempotence
               , QC.testProperty "or idempotent"                  orIdempotence
               , QC.testProperty "eq idempotent"                  eqIdempotence

               , QC.testProperty "and distributes over or"        andOverOr
               , QC.testProperty "or distributes over and"        orOverAnd

               , QC.testProperty "and absorbs or"                 andAbsorbsOr
               , QC.testProperty "or absorbs and"                 orAbsorbsAnd
               ]

------------------------------ Boolean Laws ------------------------------------
-- | shorthand type synonyms for properties over binary relations
type UnaryProperty  = OnlyBools -> QC.Property

-- | shorthand type synonyms for properties over binary relations
type BinaryProperty  = OnlyBools -> OnlyBools -> QC.Property

-- | shorthand type synonyms for properties over ternary relations
type TernaryProperty = OnlyBools -> OnlyBools -> OnlyBools -> QC.Property


---------------------------- Special Cases -------------------------------------
andAnnhilates :: UnaryProperty
andAnnhilates (unOnlyBools -> p) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (p &&& false)
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels false
     return $! left == right

orAnnhilates :: UnaryProperty
orAnnhilates (unOnlyBools -> p) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defNoModels (p ||| true)
     right <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defNoModels true
     -- note that we need to check the unsat count because the sat cnt could
     -- change depending on if we have a choice:
     -- chc D a b ||| true
     -- could have 2 models but
     -- true, only has one
     -- in this case annilation is still preserved
     return $! left == right

andIdentity :: UnaryProperty
andIdentity (unOnlyBools -> p) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (p &&& true)
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels p
     return $! left == right

orIdentity :: UnaryProperty
orIdentity (unOnlyBools -> p) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (p ||| false)
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels p
     return $! left == right

andOverOr :: TernaryProperty
andOverOr (unOnlyBools -> p) (unOnlyBools -> q) (unOnlyBools -> r) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (p &&& (q ||| r))
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels ((p &&& q) ||| (p &&& r))
     return $! left == right

orOverAnd :: TernaryProperty
orOverAnd (unOnlyBools -> p) (unOnlyBools -> q) (unOnlyBools -> r) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (p ||| (q &&& r))
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels ((p ||| q) &&& (p ||| r))
     return $! left == right


andAbsorbsOr :: BinaryProperty
andAbsorbsOr (unOnlyBools -> p) (unOnlyBools -> q) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defNoModels (p &&& (p ||| q))
     right <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defNoModels p
     -- same problem as testing annhilation
     return $! left == right

orAbsorbsAnd :: BinaryProperty
orAbsorbsAnd (unOnlyBools -> p) (unOnlyBools -> q) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defNoModels (p ||| (p &&& q))
     right <- liftIO $ unCounter . fUnSatCnt <$> solveForDiagnostics Nothing defNoModels p
     -- same problem as testing annhilation
     return $! left == right

---------------------------- General Cases -------------------------------------
doubleNegation :: UnaryProperty
doubleNegation (unOnlyBools -> p) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels p
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (bnot $ bnot p)
     return $! left == right

commutes :: (Proposition -> Proposition -> Proposition) -> BinaryProperty
commutes operator (unOnlyBools -> p) (unOnlyBools -> q) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (p `operator` q)
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (q `operator` p)
     return $! left == right

associates :: (Proposition -> Proposition -> Proposition) -> TernaryProperty
associates operator (unOnlyBools -> p) (unOnlyBools -> q) (unOnlyBools -> r) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels ((p `operator` q) `operator` r)
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (p `operator` (q `operator` r))
     return $! left == right

idempotence :: (Proposition -> Proposition -> Proposition) -> UnaryProperty
idempotence operator (unOnlyBools -> p) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels (p `operator` p)
     right <- liftIO $ unCounter . fSatCnt <$> solveForDiagnostics Nothing defNoModels p
     return $! left == right


andCommutativity :: BinaryProperty
andCommutativity = commutes (&&&)

orCommutativity :: BinaryProperty
orCommutativity = commutes (|||)

eqCommutativity :: BinaryProperty
eqCommutativity = commutes (<=>)

xorCommutativity :: BinaryProperty
xorCommutativity = commutes (<+>)


andAssociativity :: TernaryProperty
andAssociativity = associates (&&&)

orAssociativity :: TernaryProperty
orAssociativity  = associates (|||)

eqAssociativity :: TernaryProperty
eqAssociativity = associates (<=>)

xorAssociativity :: TernaryProperty
xorAssociativity = associates (<+>)


andIdempotence :: UnaryProperty
andIdempotence = idempotence (&&&)

orIdempotence :: UnaryProperty
orIdempotence = idempotence (|||)

eqIdempotence :: UnaryProperty
eqIdempotence = idempotence (<=>)
