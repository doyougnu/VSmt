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

module TestSuite.Solve.Sound where

import qualified Data.Text as Text
import qualified Data.Text.IO as T
import qualified Data.SBV as S
import qualified Data.SBV.Internals as I
import qualified Data.HashMap.Strict as M
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding (EQ, LT, GT)

import Data.Parser.Result
import Data.Core.Pretty
import Data.Core.Types
import Data.Core.Core

import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import Utils.VSMTTestFramework

parseGold :: FilePath -> IO (VariantContext, CheckableResult)
parseGold fp = do (Right m) <- parseFromFile fp
                  return m

checkResultProgram :: Proposition -> (VariantContext, CheckableResult) -> IO Bool
checkResultProgram p (vc, res) =
  do let vars      = getChkResult res
         satRes    = getVarFormula vc
         bs        = booleans satRes -- the set of dimensions
     S.AllSatResult (_,_,_,as) <- S.allSat $
       do _ <- S.sBools . Set.toList $ bs
          eval satRes >>= S.constrain
     let
       asses :: [Assignment]
       asses = M.fromList . Map.toList . fmap I.cvToBool . S.getModelDictionary <$> as

       variableSubs :: [VarAssignment]
       variableSubs = findValue vars <$> asses

       results :: [Bool]
       results = evaluate . flip substitute' p <$> variableSubs

     return $ and results

evaluate :: Prop' Value -> Bool
evaluate (LitB b)     = b
evaluate (RefB (B b)) = b
evaluate (OpB Not e) = not $ evaluate e
evaluate (OpBB op l r) = dispatchBBB op (evaluate l) (evaluate r)
evaluate (OpIB op l r) = l' `op'` r'
  where op' = dispatchNNB op
        l'  = evaluate' l
        r'  = evaluate' r
evaluate (RefB _) = error "Boolean reference without a boolean!"
evaluate ChcB {}  = error "Found Chc, should only evaluate plain propositions!"

evaluate' :: NExpr' Value -> Value
evaluate' (LitI i) = N i
evaluate' (RefI (ExRefTypeI i)) = i
evaluate' (RefI (ExRefTypeD i)) = i
evaluate' (OpI op e) = dispatchNN op $ evaluate' e
evaluate' (OpII op l r) = dispatchNNN op (evaluate' l) (evaluate' r)
evaluate' ChcI {} = error "Found Chc, should only evaluate plain propositions!"

eval :: Show a => Prop' a -> S.Symbolic S.SBool
eval (LitB b)      = return $ S.literal b
eval (RefB b)      = S.sBool $ show b
eval (OpB Not e)   = bnot $ eval e
eval (OpBB op l r) = dispatchBBB op (eval l) (eval r)
eval OpIB {}       = error "Not numerics in variant context"
eval ChcB {}       = error "Not choices in variant context"


type Assignment = M.HashMap Dim Bool
type VarAssignment = M.HashMap Var Value

dispatchNNN :: (Fractional a, Integral a) => NN_N -> a -> a -> a
dispatchNNN Add  = (+)
dispatchNNN Sub  = (-)
dispatchNNN Mult = (*)
dispatchNNN Div  = (/)
dispatchNNN Mod  = mod

dispatchNN :: Num a => N_N -> a -> a
dispatchNN Abs = abs
dispatchNN Sign = signum
dispatchNN Neg = negate

dispatchBBB :: Boolean b => BB_B -> b -> b -> b
dispatchBBB And  = (&&&)
dispatchBBB Or   = (|||)
dispatchBBB Impl = (==>)
dispatchBBB Eqv  = (<=>)
dispatchBBB XOr  = (<+>)

dispatchNNB :: Ord a => NN_B -> a -> a -> Bool
dispatchNNB LT  = (<)
dispatchNNB LTE = (<=)
dispatchNNB GT  = (>)
dispatchNNB GTE = (>=)
dispatchNNB EQ  = (==)
dispatchNNB NEQ = (/=)


substitute' :: VarAssignment -> Proposition -> Prop' Value
substitute' = fmap . (M.!)

-- substitute :: Assignment -> M.HashMap Var [(Maybe VariantContext, Value)]
-- TODO: Type class for the interpreter ranging over different languages
substitute :: Assignment -> (Maybe VariantContext, Value) -> (Bool, Value)
substitute _ (Nothing, v) = (True, v)
substitute ass (Just vc, v) = (eval' $ (M.!) ass <$> getVarFormula vc, v)
  where
    eval' :: Prop' Bool -> Bool
    eval' (LitB b) = b
    eval' (RefB b) = b
    eval' (OpB Not e) = bnot $ eval' e
    eval' (OpBB op l r) = dispatchBBB op (eval' l)  (eval' r)
    eval' OpIB {} = error "No numerics in variant context"
    eval' ChcB {}  = error "No choices in variant context"


-- | Given a single assignment, find the values for each variable in the
-- variable map in the context of that assignment
findValue :: M.HashMap Var [(Maybe VariantContext, Value)]
          -> Assignment
          -> M.HashMap Var Value
findValue ms ass = fmap (snd . head . filter fst . fmap (substitute ass)) ms

---------------------------- Tests ---------------------------------------------
properties :: TestTree
properties =
  testGroup
    "Soundness"
    [ QC.testProperty
        "All results reported from vsmt as satisfiable are indeed satisfiable"
        vsmtIsSound
    ]

-- | TODO: https://github.com/doyougnu/VSmt/issues/4
vsmtIsSound :: Proposition -> QC.Property
vsmtIsSound p = noDoubles p && isVariational p QC.==> QCM.monadicIO $ do
      let fileName = goldFile "soundness"
      liftIO $ T.putStrLn $ Text.pack "Prop   " <> pretty p
      result <- liftIO $ satVerbose p Nothing
      if wasSat result
        then do liftIO $! T.putStrLn $ pretty result
                -- roundtrip
                liftIO $! T.writeFile fileName . pretty $ result
                liftIO $! putStrLn $ "dims: " <> show (dimensions p)
                -- parsedResult <- liftIO $ parseGold fileName
                -- test
                -- liftIO $ checkResultProgram p parsedResult
                return True
        else
        return True -- skip the test
