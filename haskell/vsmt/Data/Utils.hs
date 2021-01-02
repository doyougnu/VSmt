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

module Utils where

import qualified Control.Monad.State as St
import qualified Data.Map            as M
import qualified Data.SBV            as S
import           Data.SBV.Internals  (cvToBool)

import           Data.String         (IsString (..))

import           Core.Types


type SimpleCache a m b = St.StateT (M.Map a S.SBool) m b

renameDims :: (a -> b) -> Prop' a -> Prop' b
renameDims = fmap

genConfigPool :: VariantContext -> IO [PartialConfig]
genConfigPool p =
  do
    let p' = getVarFormula p
    S.AllSatResult _ _ _ _ allRes <- S.allSat $ eval $ Plain p'
    let resMaps = S.getModelDictionary <$> allRes
        maps = M.foldMapWithKey (\k a -> M.singleton (Dim $ fromString k) (cvToBool a)) <$> resMaps
    return (fmap (\m k -> M.lookup k m) maps)

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
                              let o = udispatchop op
                              return $! o l' r'
evalPlain (ChcB {}) = error "no choices here!"
evalPlain (OpIB {}) = error "Type Chef throws smt problems?"

udispatchop :: Boolean b => BB_B -> b -> b -> b
udispatchop And  = (&&&)
udispatchop Or   = (|||)
udispatchop Impl = (==>)
udispatchop Eqv  = (<=>)
udispatchop XOr  = (<+>)

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
