-----------------------------------------------------------------------------
-- |
-- Module    : Utils
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Helpful utilities for core and solving
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Utils where

import qualified Data.Map            as M
import qualified Data.SBV            as S
import qualified Control.Monad.State as St

import           Data.SBV.Internals (cvToBool)
import           Data.String        (IsString(..))

import           Core.Types
import           Solve

type SimpleCache a m b = St.StateT (M.Map a S.SBool) m b

genConfigPool :: Maybe VariantContext -> IO [M.Map Dim Bool]
genConfigPool Nothing  = return mempty
genConfigPool (Just p) =
  do
    let p' = getVarFormula p
    S.AllSatResult _ _ _ _ allRes <- S.allSat $ eval $ Plain p'
    let resMaps = S.getModelDictionary <$> allRes
    return $ M.foldMapWithKey (\k a -> M.singleton (Dim $ fromString k) (cvToBool a)) <$> resMaps

eval :: (Show a, Ord a) => Plain (Prop' a) -> S.Symbolic S.SBool
eval = flip St.evalStateT mempty . evalPlain . unPlain

evalPlain :: (Show a, Ord a) => Prop' a -> SimpleCache a S.Symbolic S.SBool
evalPlain (LitB True)     = return S.sTrue
evalPlain (LitB False)    = return S.sFalse
evalPlain (RefB b)        = do st <- St.get
                               case M.lookup b st of
                                 Just x  -> return x
                                 Nothing -> do
                                   newSym <- St.lift $ S.sBool (show b)
                                   St.modify' (M.insert b newSym)
                                   return newSym
evalPlain (OpB _ e)       = S.sNot <$> evalPlain e
evalPlain (OpBB op l r)  = do l' <- evalPlain l
                              r' <- evalPlain r
                              let o = dispatchOp op
                              return $! o l' r'
evalPlain (ChcB {}) = error "no choices here!"
evalPlain (OpIB {}) = error "Type Chef throws smt problems?"


vCoreMetrics :: a
vCoreMetrics = error "vcore metrics"

selectVariant :: a
selectVariant = error "select variant"


selectVariantTotal :: a
selectVariantTotal = error "select variant total"

renameDims :: (a -> b) -> Prop' a -> Prop' b
renameDims = fmap
