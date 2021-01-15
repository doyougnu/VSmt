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
import qualified Data.Text.IO as T
import qualified Data.Text    as T

import Solve
import Utils.VSMTTestFramework

properties :: TestTree
properties =
  testGroup
    "Booleans" [ -- QC.testProperty "double negation is no negation" doubleNegation
                 testCase "Double Negation produces same results" dubUnitTest
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
     one <- liftIO $ getSatisfiableVCs <$> solve Nothing defSettings p
     two <- liftIO $ getSatisfiableVCs <$> solve Nothing defSettings (bnot $ bnot p)
     liftIO $ putStrLn "--------------------- Start  -----------------------------------"
     liftIO $ T.putStrLn $ "Double Negation Result: " <> T.pack (show right)
     liftIO $ T.putStrLn $ "No Negation Result: "     <> T.pack (show left)
     liftIO $! putStrLn $ "Variational Model: " <> show  (show one)               <> "\n"
     liftIO $! putStrLn $ "Plain       Result: " <> show (show two) <> "\n"
     liftIO $ putStrLn "--------------------- End    -----------------------------------"
     return $! left == right

t :: Proposition
t =  OpBB And (OpB Not (ChcB (Dim "CC") (RefB "al") (RefB "d"))) (OpB Not (OpB Not (RefB "tlvdecaxzvxcwxah")))

o :: Proposition
o = OpBB XOr (OpBB And (RefB "hjnj") (RefB "redcgptjodyiqtikjd")) (ChcB (Dim "DD") (OpB Not (RefB "wdnfikoi")) (OpBB Impl (RefB "yeicaczuyjitmx") (RefB "yysqvyrmodoj")))

n :: Proposition
n = OpBB And (OpB Not (RefB "b")) (ChcB (Dim "BB") (OpBB Or (RefB "kldt") (RefB "t")) (RefB "rdrgt"))

n' :: Proposition
n' = OpBB And (OpB Not (RefB "b")) (ChcB (Dim "BB") (OpB Not (RefB "t")) (RefB "rdrgt"))

dubUnitTest :: Assertion
dubUnitTest = do res' <- mapM go tests
                 mapM go' tests >>= putStrLn . ("\nFinal results" ++) . show
                 putStrLn $ "\nSat Counts: " ++ show res'
                 -- let res = all (==2) res'
                 assertBool "double negation" False
  where nnn = bnot $ bnot n'
        -- go = fmap (unCounter . fSatCnt) . solveGetDiag Nothing defSettings
        go = solveGetDiag Nothing defSettings
        go' = fmap getSatisfiableVCs . solve Nothing defSettings
        tests = [nnn,nnn,nnn,nnn]
