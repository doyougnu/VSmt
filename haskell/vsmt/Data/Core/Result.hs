-----------------------------------------------------------------------------
-- |
-- Module    : Core.Result
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Result module implementing variational smt models for the vsmt library
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Core.Result where

import           Control.Monad.IO.Class (MonadIO)

import           Control.DeepSeq        (NFData,force)
import           Data.Hashable          (Hashable)
import           Data.Text              (Text, unpack)
import           Data.String            (fromString)
import           Data.Maybe             (isJust)
import           GHC.Generics           (Generic)
import           Data.Map               (toList)

import qualified Data.HashMap.Strict    as M
import qualified Data.Sequence          as Seq

import qualified Data.SBV.Control       as C
import qualified Data.SBV.Internals     as I
import qualified Data.SBV.Trans         as S
import qualified Data.SBV.Trans.Control as T


import           Core.Pretty
import           Core.Types
import           Core.Utils             ((:/\) (..))


-- | An SMT Result formula, spine strict, we leave values lazy because they may
-- not be needed. We will be incrementally building these formulas through
-- conses and will not be doing any other real operations until the solver is
-- finished. Thus a list is an appropriate choice. We store the variantContext
-- with a value returned from SBV so that we can control the output to the user.
-- You should view this as an internal datatype, in the average case this will
-- be transformed into a Map of "variable" -> SMTLIB2 program where the SMTLIB2
-- program will dispatch the right value based on the values of dimensions
newtype ResultFormula = ResultFormula (Seq.Seq (Maybe VariantContext :/\ I.CV))
    deriving (Eq,Show,Semigroup,Monoid,Generic,NFData)

-- | We store the raw output from SBV (I.CV) to avoid having to use existentials
-- and to leverage the instance already made in the SBV library. This leads to
-- cleaner implementation on our end.
newtype VariableMap d = VariableMap { getVMap :: M.HashMap d ResultFormula }
  deriving (Eq,Show,Generic,NFData)

-- | newtype wrapper for better printing
newtype Result = Result { unboxResult :: Result' Var} deriving (Eq,Generic,NFData)


data Result' d = Result' { variables      :: VariableMap d
                         , satisfiableVCs :: Maybe VariantContext
                         -- , usSatisfiableVCs :: Maybe VariantContext
                         } deriving (Eq, Show,Generic)

instance (Eq d, Hashable d, NFData d) => Semigroup (VariableMap d) where
  (VariableMap !l) <> (VariableMap !r) = VariableMap $! M.unionWith go' l r
    where go' !l' !r' = l' <> r'

deriving instance (Eq d, Hashable d, NFData d) => Monoid (VariableMap d)

instance Pretty (Result' Var) where
  pretty r = "=: Model := " <> newline <>
             pretty (variables r) <>
             newline <>
             "=: Sat_Model := " <>
             newline <>
             pretty (satisfiableVCs r) <>
             newline

-- TODO: use Text.PrintF from base for better formatted output
deriving instance Pretty d => Pretty (VariableMap d)
instance Pretty (Seq.Seq (Maybe VariantContext :/\ I.CV)) where pretty = prettyResultFormula
instance Pretty d => Pretty (M.HashMap d ResultFormula) where
  pretty = M.foldMapWithKey (\k v -> pretty k <> "   -->   " <> pretty v <> newline)

-- | We use a Maybe to form the mempty for monoid, this leads to prettier pretty
-- printing
prettyResultFormula :: Seq.Seq (Maybe VariantContext :/\ I.CV) -> Text
prettyResultFormula Seq.Empty = mempty
prettyResultFormula ((Nothing :/\ val) Seq.:<| Seq.Empty)  = pretty val
prettyResultFormula ((Just !ctx :/\ val) Seq.:<| Seq.Empty) =
  parens $!
  "ite" <> space <> pretty ctx <> space <> parens (pretty val) <> space <> done
  where done = "Undefined"
prettyResultFormula ((Nothing :/\ val) Seq.:<| xs) =
  parens $! "ite " <>
  parens "True :: Bool" <> space <>
  parens (pretty val) <> space <>
  pretty xs
prettyResultFormula ((Just !ctx :/\ val) Seq.:<| xs) =
  parens $!
  "ite " <>
  pretty ctx <> space <>
  parens (pretty val) <> space <>
  pretty xs

instance Pretty ResultFormula where pretty (ResultFormula x) = pretty x

instance Semigroup Result where (Result !a) <> (Result !b) = Result (a <> b)
deriving instance Monoid Result

instance Show Result where show = unpack . pretty

instance Pretty Result where pretty (Result r) = pretty r

instance NFData d => NFData (Result' d)

instance (Eq d, Hashable d, NFData d) => Semigroup (Result' d) where
  (<>) Result' {variables=(!vl),satisfiableVCs=(!sl)} Result'{variables=(!vr),satisfiableVCs=(!sr)}
    = Result' { variables = vl <> vr, satisfiableVCs = sl <> sr}

instance (Eq d, Hashable d, NFData d) => Monoid (Result' d) where
  mempty = Result'{variables=mempty,satisfiableVCs=mempty}
  mappend !x !y = x <> y

-- | get satisfiable VCs from a result
getSatisfiableVCs :: Result -> Maybe VariantContext
getSatisfiableVCs = satisfiableVCs . unboxResult

-- | check if the current context is sat or not
isSat :: C.Query (Bool :/\ Result)
isSat = do cs <- C.checkSat
           return $! case cs of
                       C.Sat -> True  :/\ mempty
                       _     -> False :/\ mempty

wasSat :: Result -> Bool
wasSat = isJust . satisfiableVCs . unboxResult

-- | Generate a VSMT model
getVSMTModel :: (T.MonadQuery m, MonadIO m) => m S.SMTResult
getVSMTModel = force <$> T.getSMTResult

-- TODO: https://github.com/doyougnu/VSmt/issues/5
-- | Get a VSMT model in any supported monad.
getResult :: (MonadIO m, T.MonadQuery m) => Maybe VariantContext -> m (Bool :/\ Result)
getResult !vf =
  do !model <- getVSMTModel
     return $!
       case model of
         m@(S.Satisfiable _ _)         ->
        -- when satisfiable we get the model dictionary, turn it into a
        -- result map and then insert the config proposition that created the
        -- satisfiable result into the __Sat element of the map
           let !gMD = S.getModelDictionary $! m
               xs = toList gMD
               res = toResMap xs
               in  True :/\ Result res
      -- (S.Unsatisfiable _ unsatCore) ->
        -- we apply f to True here because in the case of an unsat we want to
        -- save the proposition that produced the unsat, if we applied to
        -- false then we would have the negation of that proposition for
        -- unsat
        -- unSatToResult (f True) $ fromMaybe mempty unsatCore
         _                           -> False :/\ mempty
 where
   toResMap !m' =
     -- Result' {variables = VariableMap $! M.foldMapWithKey
     --          (\(!k) (!a) -> M.singleton (fromString k) (ResultFormula $! pure (vf, a))) m'
     --         ,satResult=vf}
     Result' {variables = VariableMap $!
                          M.fromList $!
                          fmap (\(!k,!v) -> (fromString k, ResultFormula $! pure (vf :/\ v))) m'
             ,satisfiableVCs=vf}
