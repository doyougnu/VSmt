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
               ]

-- vsmtIsSoundBools :: OnlyBools -> QC.Property
-- vsmtIsSoundBools (unOnlyBools -> p) = noNestedChoices p && isVariational p QC.==> QCM.monadicIO $
--   do
--     liftIO $ putStrLn "--------------------- Start  -----------------------------------"
--     liftIO $ T.putStrLn $ "Proposition: " <> pretty p
--     plainRes <- liftIO $! filter sSnd <$> vOnPByConfig p
--     !res    <- liftIO $! unCounter . fSatCnt <$> solveGetDiag Nothing defSettings p
--     !res2    <- liftIO $! getSatisfiableVCs <$> solve Nothing defSettings p
--     liftIO $! putStrLn $ "Variational Result: " <> show res               <> "\n"
--     liftIO $! putStrLn $ "Variational Model: " <> show (length res2)               <> "\n"
--     liftIO $! putStrLn $ "Plain       Result: " <> show (length plainRes) <> "\n"
--     liftIO $ putStrLn "--------------------- End    -----------------------------------"
--     return $ res `seq` res == length plainRes


------------------------------- Boolean Laws -----------------------------------
doubleNegation :: OnlyBools -> QC.Property
doubleNegation (unOnlyBools -> p) = QCM.monadicIO $
  do left  <- liftIO $ unCounter . fSatCnt . sSnd <$> solveGetDiag Nothing defSettings p
     right <- liftIO $ unCounter . fSatCnt . sSnd <$> solveGetDiag Nothing defSettings (bnot $ bnot p)
     return $! left == right
