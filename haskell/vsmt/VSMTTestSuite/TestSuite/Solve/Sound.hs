-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Solve.Sound
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases to ensure the solver is sound up to sbv and consequently smtlib2
-- solvers
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module TestSuite.Solve.Sound where

-- import qualified Data.Text as Text
import qualified Data.Text.IO as T
-- import qualified Data.SBV as S
-- import qualified Data.SBV.Internals as I
-- import qualified Data.HashMap.Strict as M
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
-- import Prelude hiding (EQ, LT, GT)
-- import qualified Data.List as L

-- import Parser.Result
-- import Core.Pretty
-- import Core.Types
-- import Core.Core
-- import Core.Utils

import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import Utils.VSMTTestFramework

-- parseGold :: FilePath -> IO (VariantContext, CheckableResult)
-- parseGold fp = do m <- parseFromFile fp
--                   case m of
--                     Left x -> error $ "Something with the parser: " ++ show x ++ "\n and the " ++ show m
--                     Right x -> return x

-- checkResultProgram :: Proposition -> (VariantContext, CheckableResult) -> IO Bool
-- checkResultProgram p (vc, res) =
--   do let vars      = getChkResult res
--          satRes    = getVarFormula vc
--          bs :: [String]
--          bs = fmap show . Set.toList $ booleans satRes -- the set of dimensions
--      putStrLn $ "Sat Result: " ++ show satRes
--      S.AllSatResult _ _ _ _ as <- S.allSat $
--        do symBs <- mapM S.sBool bs
--           let sBs = M.fromList $ zip (toDim <$> bs) symBs
--           eval satRes sBs >>= S.constrain
--      let
--        help k v = Map.singleton (toDim k) (I.cvToBool v)
--        dicts = S.getModelDictionary <$> as

--        -- assignments of dimensions
--        asses :: [Assignment]
--        asses = M.fromList . Map.toList . Map.foldMapWithKey help <$> dicts

--        -- assignments of variables
--        variableSubs :: [VarAssignment]
--        variableSubs = findValue vars <$> asses

--        -- plain propositions that no longer have choices
--        plains :: [Proposition]
--        plains = flip configure p <$> fmap (M.!) asses

--        -- plain proposiitons that no longer have references
--        plainsFs :: [Prop' Value]
--        plainsFs = zipWith substitute' variableSubs plains
--          --flip substitute' p <$> plains

--        results :: [Bool]
--        results = evaluate <$> plainsFs

--      putStrLn $ "Asses: " ++ show asses
--      putStrLn ""
--      putStrLn $ "Subs0: " ++ show variableSubs
--      putStrLn ""
--      putStrLn $ "Plains: " ++ show plainsFs
--      putStrLn ""
--      putStrLn $ "Results: " ++ show results
--      putStrLn ""

--      return $ and results

-- evaluate :: Prop' Value -> Bool
-- evaluate (LitB b)     = b
-- evaluate (RefB (B b)) = b
-- evaluate (OpB Not e) = not $ evaluate e
-- evaluate (OpBB op l r) = dispatchBBB op (evaluate l) (evaluate r)
-- evaluate (OpIB op l r) = case (l', r') of
--                            (N (I i), N (I j)) -> fromIntegral i `op'` fromIntegral j
--                            (N (D i), N (I j)) -> i `op'` fromIntegral j
--                            (N (I i), N (D j)) -> fromIntegral i `op'` j
--                            (N (D i), N (D j)) -> i `op'` j
--                            _                  -> error "Bools in relation"
--   where op' = dispatchNNB op
--         l'  = evaluate' l
--         r'  = evaluate' r
-- evaluate (RefB _) = error "Boolean reference without a boolean!"
-- evaluate ChcB {}  = error "Found Chc, should only evaluate plain propositions!"

-- evaluate' :: NExpr' Value -> Value
-- evaluate' (LitI i) = N i
-- evaluate' (RefI (ExRefTypeI i)) = i
-- evaluate' (RefI (ExRefTypeD i)) = i
-- evaluate' (OpI op e) = dispatchNN op $ evaluate' e
-- evaluate' (OpII op l r) = dispatchNNN op (evaluate' l) (evaluate' r)
-- evaluate' ChcI {} = error "Found Chc, should only evaluate plain propositions!"

-- eval :: Prop' Dim -> SAssignment -> S.Symbolic S.SBool
-- eval (LitB b)      _  = return $ S.literal b
-- eval (RefB b)      m  = return $ (M.!) m b
-- eval (OpB Not e)   m  = bnot $ eval e m
-- eval (OpBB op l r) m  = dispatchBBB op (eval l m) (eval r m)
-- eval OpIB {}       _  = error "Not numerics in variant context"
-- eval ChcB {}       _  = error "Not choices in variant context"


-- type Assignment = M.HashMap Dim Bool
-- type SAssignment = M.HashMap Dim S.SBool
-- type VarAssignment = M.HashMap Var Value

-- dispatchNNN :: (Fractional a, Integral a) => NN_N -> a -> a -> a
-- dispatchNNN Add  = (+)
-- dispatchNNN Sub  = (-)
-- dispatchNNN Mult = (*)
-- dispatchNNN Div  = (/)
-- dispatchNNN Mod  = mod

-- dispatchNN :: Num a => N_N -> a -> a
-- dispatchNN Abs = abs
-- dispatchNN Sign = signum
-- dispatchNN Neg = negate

-- dispatchBBB :: Boolean b => BB_B -> b -> b -> b
-- dispatchBBB And  = (&&&)
-- dispatchBBB Or   = (|||)
-- dispatchBBB Impl = (==>)
-- dispatchBBB Eqv  = (<=>)
-- dispatchBBB XOr  = (<+>)

-- dispatchNNB :: Ord a => NN_B -> a -> a -> Bool
-- dispatchNNB LT  = (<)
-- dispatchNNB LTE = (<=)
-- dispatchNNB GT  = (>)
-- dispatchNNB GTE = (>=)
-- dispatchNNB EQ  = (==)
-- dispatchNNB NEQ = (/=)


-- substitute' :: VarAssignment -> Proposition -> Prop' Value
-- substitute' = fmap . (M.!)

-- substitute :: Assignment -> M.HashMap Var [(Maybe VariantContext, Value)]
-- TODO: Type class for the interpreter ranging over different languages
-- substitute :: Assignment -> (Maybe VariantContext, Value) -> (Bool, Value)
-- substitute _ (Nothing, v) = (True, v)
-- substitute ass (Just vc, v) = (eval' $ (M.!) ass <$> getVarFormula vc, v)
--   where
--     eval' :: Prop' Bool -> Bool
--     eval' (LitB b) = b
--     eval' (RefB b) = b
--     eval' (OpB Not e) = bnot $ eval' e
--     eval' (OpBB op l r) = dispatchBBB op (eval' l)  (eval' r)
--     eval' OpIB {} = error "No numerics in variant context"
--     eval' ChcB {}  = error "No choices in variant context"


-- -- | Given a single assignment, find the values for each variable in the
-- -- variable map in the context of that assignment
-- findValue :: M.HashMap Var [(Maybe VariantContext, Value)]
--           -> Assignment
--           -> M.HashMap Var Value
-- findValue ms ass = final
--   where subbed = fmap (fmap (substitute ass)) ms -- make the subs
--         filtered = fmap (filter fst) subbed      -- perform the selection on choices
--         final = fmap (snd . head) . M.filter (not . L.null) $ filtered -- remove nulls

-- findValue' :: M.HashMap Var [(Maybe VariantContext, Value)]
--           -> Assignment
--           -> M.HashMap Var [(Bool,Value)]
-- findValue' ms ass = fmap (fmap (substitute ass)) ms
---------------------------- Tests ---------------------------------------------
properties :: TestTree
properties =
  testGroup
    "Soundness"
    [ QC.testProperty
        "All results reported from vsmt as satisfiable are indeed satisfiable"
        vsmtIsSoundBools
    ]

-- | Check the soundness of the solver, we are checking against vOnP which
-- doesn't use any of the vsmt code. The comparison is thus hard because of
-- different domains, to simplify we check the number of sat is consistent
-- between the two for all variants. The models are hard to check because of
-- z3's default behavior with get-model (i.e., it doesn't return values for
-- variables that are not needed to make a formula satisfiable).
vsmtIsSoundBools :: OnlyBools -> QC.Property
vsmtIsSoundBools (unOnlyBools -> p) = noNestedChoices p && isVariational p QC.==> QCM.monadicIO $
  do
    liftIO $ putStrLn "--------------------- Start  -----------------------------------"
    liftIO $ T.putStrLn $ "Proposition: " <> pretty p
    plainRes <- liftIO $! filter sSnd <$> vOnPByConfig p
    !res    <- liftIO $! unCounter . fSatCnt <$> solveForDiagnostics Nothing defSettings p
    !res2    <- liftIO $! getSatisfiableVCs <$> solve Nothing defSettings p
    liftIO $! putStrLn $ "Variational Result: " <> show res               <> "\n"
    liftIO $! putStrLn $ "Variational Model: " <> show (length res2)               <> "\n"
    liftIO $! putStrLn $ "Plain       Result: " <> show (length plainRes) <> "\n"
    liftIO $ putStrLn "--------------------- End    -----------------------------------"
    return $ res `seq` res == length plainRes

-- | TODO: https://github.com/doyougnu/VSmt/issues/4
-- vsmtIsSound :: Proposition -> QC.Property
-- vsmtIsSound p = hasVariables p && noDoubles p && isVariational p QC.==> QCM.monadicIO $ do
--       let fileName = goldFile "soundness"
--       liftIO $ T.putStrLn $ Text.pack "Prop   " <> pretty p
--       liftIO $ putStrLn $ "PropShow   " <> show p
--       result <- liftIO $ satVerbose p Nothing
--       if wasSat result
--         then do liftIO $! T.putStrLn $ pretty result
--                 -- roundtrip
--                 liftIO $! T.writeFile fileName . pretty $ result
--                 liftIO $! putStrLn $ "dims: " <> show (dimensions p)
--                 parsedResult <- liftIO $ parseGold fileName
--                 -- test
--                 liftIO $! putStrLn $ "Parsed Result: " <> show parsedResult <> "\n"
--                 liftIO $ checkResultProgram p parsedResult
--                 -- return True
--         else return True -- skip the test

-- vsmtIsSoundBools :: Proposition -> QC.Property
-- vsmtIsSoundBools p = hasVariables p && onlyBools p QC.==> QCM.monadicIO $ do
--       let fileName = goldFile "Boolean_soundness"
--       liftIO $ T.putStrLn $ Text.pack "Prop   " <> pretty p
--       liftIO $ putStrLn $ "PropShow   " <> show p
--       result <- liftIO $ satVerbose p Nothing
--       if wasSat result
--         then do liftIO $! T.putStrLn $ pretty result
--                 -- roundtrip
--                 liftIO $! T.writeFile fileName . pretty $ result
--                 liftIO $! putStrLn $ "dims: " <> show (dimensions p)
--                 parsedResult <- liftIO $ parseGold fileName
--                 -- test
--                 liftIO $! putStrLn $ "Parsed Result: " <> show parsedResult <> "\n"
--                 liftIO $ checkResultProgram p parsedResult
--                 -- return True
--         else return True -- skip the test
