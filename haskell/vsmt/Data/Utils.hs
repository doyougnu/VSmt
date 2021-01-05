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
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Utils where

import qualified Control.Monad.State as St
import qualified Data.Map            as M
import qualified Data.SBV            as S
import           Data.SBV.Internals  (cvToBool)

import           Data.String         (IsString (..))
import           Data.Text           (pack)
import           Z3.Monad            as Z

import           Core.Types
import           Core.Core
import qualified Solve               as Sl
import           Settings            (defSettings)


type SimpleCache a m b = St.StateT (M.Map a S.SBool) m b
type SimpleZCache a m b = St.StateT (M.Map a Sl.SBool) m b

genConfigPool :: VariantContext -> IO [PartialConfig]
genConfigPool p =
  do
    let p' = getVarFormula p
    S.AllSatResult _ _ _ _ allRes <- S.allSat $ evalSBVPlain $ Plain p'
    let resMaps = S.getModelDictionary <$> allRes
        maps = M.foldMapWithKey (\k a -> M.singleton (Dim $ fromString k) (cvToBool a)) <$> resMaps
    return (fmap (flip M.lookup) maps)

evalSBVPlain :: (Show a, Ord a) => Plain (Prop' a) -> S.Symbolic S.SBool
evalSBVPlain = flip St.evalStateT mempty . evalSBV . unPlain

evalSBV :: (Show a, Ord a) => Prop' a -> SimpleCache a S.Symbolic S.SBool
evalSBV (LitB True)     = return S.sTrue
evalSBV (LitB False)    = return S.sFalse
evalSBV (RefB b)        = do st <- St.get
                             case M.lookup b st of
                               Just x  -> return x
                               Nothing -> do
                                 newSym <- St.lift $ S.sBool (show b)
                                 St.modify' (M.insert b newSym)
                                 return newSym
evalSBV (OpB _ e)       = S.sNot <$> evalSBV e
evalSBV (OpBB op l r)  = do l' <- evalSBV l
                            r' <- evalSBV r
                            let o = udispatchop op
                            return $! o l' r'
evalSBV ChcB {} = error "no choices here!"
evalSBV OpIB {} = error "Type Chef throws smt problems?"

udispatchop :: Boolean b => BB_B -> b -> b -> b
udispatchop And  = (&&&)
udispatchop Or   = (|||)
udispatchop Impl = (==>)
udispatchop Eqv  = (<=>)
udispatchop XOr  = (<+>)


runEvalZ3 :: (Show a, Ord a) => Plain (Prop' a) -> Z.Z3 Sl.SBool
runEvalZ3 = flip St.evalStateT mempty . evalZPlain . unPlain

evalZPlain :: (Show a, Ord a) => Prop' a -> SimpleZCache a Z.Z3 Sl.SBool
evalZPlain (LitB True)     = St.lift $ Sl.sTrue
evalZPlain (LitB False)    = St.lift $ Sl.sFalse
evalZPlain (RefB b)        = do st <- St.get
                                case M.lookup b st of
                                  Just x  -> return x
                                  Nothing -> do
                                    newSym <- St.lift $ Sl.sBool $ pack $ show b
                                    St.modify' (M.insert b newSym)
                                    return newSym
evalZPlain (OpB _ e)       = do e' <- evalZPlain e
                                St.lift $ Sl.sNot e'
evalZPlain (OpBB op l r)  = do l' <- evalZPlain l
                               r' <- evalZPlain r
                               St.lift $ (Sl.dispatchOp op) l' r'
evalZPlain ChcB {} = error "no choices here!"
evalZPlain OpIB {} = error "Type Chef throws smt problems?"

instance Boolean S.SBool where
  true  = S.sTrue
  false = S.sFalse
  bnot  = S.sNot
  (&&&) = (S..&&)
  (|||) = (S..||)
  (<=>) = (S..<=>)
  {-# INLINE true #-}
  {-# INLINE false #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (|||) #-}
  {-# INLINE (<=>) #-}

-- | Plain propositions on the variational solver
pOnV :: Proposition -> IO [[(Maybe VariantContext, (Z.Result,[(Var,Value)]))]]
pOnV p = do
  let configs = genConfigs p
      ps = fmap (`configure` p) configs
  mapM (Sl.solve Nothing defSettings) ps

-- | Plain propositions on the plain solver
pOnP :: Proposition -> IO [(Z.Result, String)]
pOnP p = do
  let configs = genConfigs p
      ps = fmap (Plain . (`configure` p)) configs
      go prop = do s <- Sl.unSBool <$> runEvalZ3 prop
                   Z.assert s
                   (r,m) <- Z.getModel
                   m' <- maybe (pure "") Z.modelToString m
                   return (r, m')

  mapM (Z.evalZ3 . go) ps

vOnP :: Proposition -> IO [(Z.Result, String)]
vOnP p = do
  let configs = genConfigs p
      ps = fmap (Plain . (`configure` p)) configs
      go prop = Z.local $
        do s <- Sl.unSBool <$> runEvalZ3 prop
           Z.assert s
           (r,m) <- Z.getModel
           m' <- maybe (pure "") Z.modelToString m
           return (r, m')
  Z.evalZ3 $ mapM go ps
