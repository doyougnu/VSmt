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

import           Control.Monad.IO.Class (MonadIO,liftIO)

import           Control.Monad.Logger   (MonadLogger)
import           Control.DeepSeq        (NFData)
import           Data.Hashable          (Hashable)
import           Data.Text              (Text, pack, unpack)
import           GHC.Generics           (Generic)

import qualified Data.HashMap.Strict    as M
import qualified Data.Sequence          as Seq
import qualified Z3.Monad               as Z

import           Core.Pretty
import           Core.Types
import           Core.Utils             ((:/\) (..))
import           Parser.Z3

-- | An SMT Result formula, spine strict, we leave values lazy because they may
-- not be needed. We will be incrementally building these formulas through
-- conses and will not be doing any other real operations until the solver is
-- finished. Thus a list is an appropriate choice. We store the variantContext
-- with a value returned from SBV so that we can control the output to the user.
-- You should view this as an internal datatype, in the average case this will
-- be transformed into a Map of "variable" -> SMTLIB2 program where the SMTLIB2
-- program will dispatch the right value based on the values of dimensions
newtype ResultFormula = ResultFormula (Seq.Seq (Maybe VariantContext :/\ Value))
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
instance Pretty (Seq.Seq (Maybe VariantContext :/\ Value)) where pretty = prettyResultFormula
instance Pretty d => Pretty (M.HashMap d ResultFormula) where
  pretty = M.foldMapWithKey (\k v -> pretty k <> "   -->   " <> pretty v <> newline)

-- | We use a Maybe to form the mempty for monoid, this leads to prettier pretty
-- printing
prettyResultFormula :: Seq.Seq (Maybe VariantContext :/\ Value) -> Text
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
isSat :: Z.MonadZ3 m => Maybe VariantContext -> m (Bool :/\ Result)
isSat vcs = do cs <- Z.check
               return $! case cs of
                           Z.Sat -> (True :/\) $ Result $! Result' {variables=mempty
                                                                   ,satisfiableVCs=vcs
                                                                   }
                           _     -> (False :/\ mempty)

-- | Generate a VSMT model
getResult :: (Z.MonadZ3 m, MonadIO m, MonadLogger m) => Maybe VariantContext -> m (Bool :/\ Result)
getResult !vc =
  do (!r,!m) <- Z.getModel
     case r of
       Z.Unsat -> liftIO $ putStrLn "Unsat" >> return (False :/\ mempty)
       Z.Undef -> liftIO $ putStrLn "undef" >> return (False :/\ mempty)
       _       ->
         do m' <- maybe (pure mempty) Z.modelToString m
            let !ms  = parseModel . pack $! m'
                bindings = VariableMap $!
                           M.fromList $!
                           fmap (\(k :/\ v) ->
                                   (k, ResultFormula $! pure (vc :/\ v))) ms
            return $ (True :/\) . Result $! Result' {variables = bindings, satisfiableVCs = vc}
