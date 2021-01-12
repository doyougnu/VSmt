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
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TupleSections        #-}

module Utils where

import qualified Control.Monad.State as St
import qualified Data.Map            as M
import qualified Data.SBV            as S
import           Data.SBV.Internals  (cvToBool)

import           Data.String         (IsString (..))
import           Data.Text           (pack)
import           Data.Maybe          (fromJust)
import           Z3.Monad            as Z

import           Core.Types
import           Core.Core
import           Core.Result         as R (Result)
import           Core.Utils
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

-- | Given a total variant context return all variants
genVariants :: Proposition -> IO [Plain Proposition]
genVariants p =
  do let totalConfs = genConfigs p
     -- we know the props have to be plain because of the total vcs
     return $! fmap (fromJust . validatePlain . (`configure` p)) totalConfs

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

-- | Plain propositions on the variational solver testing the overhead of
-- accumulate/evaluate
pOnV :: [Plain Proposition] -> IO R.Result
pOnV =  fmap mconcat . mapM (Sl.solve Nothing defSettings) . fmap unPlain

-- | Plain propositions on the plain solver the brute force position
pOnPModel :: [Plain Proposition] -> IO [Z.Result :/\  String]
pOnPModel ps = do
  let go prop = do s <- Sl.unSBool <$> runEvalZ3 prop
                   Z.assert s
                   (r,m) <- Z.getModel
                   m' <- maybe (pure "") Z.modelToString m
                   return (r :/\  m')
  mapM (Z.evalZ3 . go) ps

pOnP :: [Plain Proposition] -> IO [Z.Result]
pOnP ps = do
  let go prop = do s <- Sl.unSBool <$> runEvalZ3 prop
                   Z.assert s
                   Z.check
  mapM (Z.evalZ3 . go) ps

-- | vOnP tests the performance of configuration. That is it should input a
-- variational formula, then configure to all its variants, then run them each
-- on a plain solver
vOnPModel :: Proposition -> IO [Z.Result :/\ String]
vOnPModel p = do
  let configs = genConfigs p
      ps = fmap (Plain . (`configure` p)) (configs)
      go prop = Z.local $
        do s <- Sl.unSBool <$> runEvalZ3 prop
           Z.assert s
           (r,m) <- Z.getModel
           m' <- maybe (pure "") Z.modelToString m
           return (r :/\  m')
  Z.evalZ3 $ mapM go ps

vOnP :: Proposition -> IO [Z.Result]
vOnP p = do
  let configs = genConfigs p
      ps = fmap (Plain . (`configure` p)) configs
      go prop = Z.local $
        do s <- Sl.unSBool <$> runEvalZ3 prop
           Z.assert s
           r <- Z.check
           return r
  Z.evalZ3 $ mapM go ps

vOnPByConfig :: Proposition -> IO [VariantContext :/\ Bool]
vOnPByConfig p = do
  let configs      = genConfigs' p
      configAsFunc = fmap (M.!) configs
      variants     = fmap (Plain . (`configure` p)) configAsFunc
      go prop  = Z.local $
        do s <- Sl.unSBool <$> runEvalZ3 prop
           Z.assert s
           r <- Z.check
           let res = case r of
                 Z.Sat -> True
                 _     -> False
           return res
  Z.evalZ3 $ mapM (\(c,plnP) -> (configToContext c :/\) <$> go plnP) $ zip configs variants
