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
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}

module Utils where

import qualified Control.Monad.State as St
import qualified Data.Map            as M

import qualified Data.SBV            as S
import qualified Data.SBV.Control    as SC
import           Data.SBV.Internals  (cvToBool)

import           Data.String         (IsString (..))
import           Data.List           (nub)
import           Data.Maybe          (fromJust)

import           Core.Types
import           Core.Core
import           Core.Result         as R (getResult,Result)
import           Core.Utils
import qualified Solve               as Sl
import           Settings            (defSettings)


type SimpleCache a m b = St.StateT (M.Map a S.SBool) m b

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

evalSBVPlainQ :: (Show a, Ord a) => Plain (Prop' a) -> SC.Query S.SBool
evalSBVPlainQ = flip St.evalStateT mempty . evalSBVQ . unPlain

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

evalSBVQ :: (Show a, Ord a) => Prop' a -> SimpleCache a SC.Query S.SBool
evalSBVQ (LitB True)     = return S.sTrue
evalSBVQ (LitB False)    = return S.sFalse
evalSBVQ (RefB b)        = do st <- St.get
                              case M.lookup b st of
                                Just x  -> return x
                                Nothing -> do
                                  newSym <- St.lift $ SC.freshVar (show b)
                                  St.modify' (M.insert b newSym)
                                  return newSym
evalSBVQ (OpB _ e)       = S.sNot <$> evalSBVQ e
evalSBVQ (OpBB op l r)  = do l' <- evalSBVQ l
                             r' <- evalSBVQ r
                             let o = udispatchop op
                             return $! o l' r'
evalSBVQ ChcB {} = error "no choices here!"
evalSBVQ OpIB {} = error "Type Chef throws smt problems?"



udispatchop :: Boolean b => BB_B -> b -> b -> b
udispatchop And  = (&&&)
udispatchop Or   = (|||)
udispatchop Impl = (==>)
udispatchop Eqv  = (<=>)
udispatchop XOr  = (<+>)


-- | Plain propositions on the variational solver testing the overhead of
-- accumulate/evaluate
pOnV :: [Plain Proposition] -> IO R.Result
pOnV =  fmap mconcat . mapM (Sl.solve Nothing defSettings) . fmap unPlain

-- | Plain propositions on the plain solver the brute force position
pOnPModel :: [Plain Proposition] -> IO [S.SatResult]
pOnPModel ps = do
  let go prop = do s <- evalSBVPlain prop
                   S.constrain s
  mapM (S.sat . go) ps

pOnP :: [Plain Proposition] -> IO [S.SatResult]
pOnP ps = do
  let go prop = do s <- evalSBVPlain prop
                   S.constrain s
  mapM (S.sat . go) ps

-- | vOnP tests the performance of configuration. That is it should input a
-- variational formula, then configure to all its variants, then run them each
-- on a plain solver
vOnPModel :: Proposition -> IO [Bool :/\ R.Result]
vOnPModel p = do
  let configs = genConfigs p
      ps = fmap (Plain . (`configure` p)) (configs)
      go prop = SC.query $
        SC.inNewAssertionStack $
        do s <- evalSBVPlainQ prop
           S.constrain s
           R.getResult mempty
  S.runSMT $ mapM go ps

vOnP :: Proposition -> IO [SC.CheckSatResult]
vOnP p = do
  let configs = genConfigs p
      ps = fmap (Plain . (`configure` p)) configs
      go prop = SC.query $ SC.inNewAssertionStack $
                do s <- evalSBVPlainQ prop
                   S.constrain s
                   SC.checkSat
  S.runSMT $ mapM go ps

vOnPByConfig :: Proposition -> IO [VariantContext :/\ Bool]
vOnPByConfig p = do
  let configs      = genConfigs' p
      configAsFunc = fmap (M.!) configs
      -- nub to remove duplicates produced by configuration. genConfigs' doesn't
      -- account for nested choices even though these map to the same variant
      -- thus we have to remove duplicates because vsmt will never evaluate the
      -- a duplicate variant produced by nested choices.
      variants     = fmap (Plain . (`configure` p)) configAsFunc
  let go prop  = SC.query $ SC.inNewAssertionStack $
        do s <- evalSBVPlainQ prop
           S.constrain s
           r <- SC.checkSat
           let res = case r of
                 SC.Sat -> True
                 _     -> False
           return res
  putStrLn $ "Config: " ++ show configs ++ "\n"
  putStrLn $ "Variants: " ++ show variants ++ "\n"
  putStrLn $ "Filtered Variants: " ++ show (nub variants) ++ "\n"
  S.runSMT $ mapM (\(c,plnP) -> (configToContext c :/\) <$> go plnP) $ zip configs (nub variants)
