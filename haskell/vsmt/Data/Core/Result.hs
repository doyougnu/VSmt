-----------------------------------------------------------------------------
-- |
-- Module    : Data.Core.Result
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Result module implementing variational smt models for the vsmt library
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.Core.Result where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Data              (Typeable)
import           Data.Hashable          (Hashable)
import           Data.Map               (toList)
import qualified Data.SBV.Control       as C
import qualified Data.SBV.Internals     as I
import qualified Data.SBV.Trans         as S
import qualified Data.SBV.Trans.Control as T
import           Data.Text              (Text,pack,unpack)
import           Data.Semigroup         ((<>))
import           Data.Char              (isSpace)
import           Data.List              (intercalate, nub)

import qualified Data.HashMap.Strict    as M
import           Data.String            (IsString, fromString)
import           GHC.Generics           (Generic)

import           Data.Core.Types
import           Data.Core.Core
import           Data.Core.Pretty

-- | An SMT Result formula, spine strict, we leave values lazy because they may
-- not be needed. We will be incrementally building these formulas through
-- conses and will not be doing any other real operations until the solver is
-- finished. Thus a list is an appropriate choice. We store the variantContext
-- with a value returned from SBV so that we can control the output to the user.
-- You should view this as an internal datatype, in the average case this will
-- be transformed into a Map of "variable" -> SMTLIB2 program where the SMTLIB2
-- program will dispatch the right value based on the values of dimensions
newtype ResultFormula = ResultFormula [(Maybe VariantContext, I.CV)]
    deriving (Eq,Show,Generic,Semigroup,Monoid)

-- | We store the raw output from SBV (I.CV) to avoid having to use existentials
-- and to leverage the instance already made in the SBV library. This leads to
-- cleaner implementation on our end.
newtype VariableMap d = VariableMap {getVMap :: M.HashMap d ResultFormula }
  deriving (Eq,Show)

instance (Eq d, Hashable d) => Semigroup (VariableMap d) where
  (VariableMap l) <> (VariableMap r) = VariableMap $ M.unionWith (<>) l r

deriving instance (Eq d, Hashable d) => Monoid (VariableMap d)

instance Pretty (Result' Var) where
  pretty r = "model := " <> newline <>
             pretty (variables r) <>
             newline <>
             "Sat_Model := " <>
             newline <>
             pretty (satResult r) <>
             newline

-- TODO: use Text.PrintF from base for better formatted output
deriving instance Pretty d => Pretty (VariableMap d)
instance Pretty d => Pretty (M.HashMap d ResultFormula) where
  pretty = M.foldMapWithKey (\k v -> pretty k <> "   -->   " <> pretty v <> newline)

-- | We use a Maybe to form the mempty for monoid, this leads to prettier pretty
-- printing
prettyResultFormula :: [(Maybe VariantContext, I.CV)] -> Text
prettyResultFormula [(Nothing, val)] = pretty val
prettyResultFormula [(ctx, val)] = parens $
  "ite" <> space <> pretty ctx <> space <> parens (pretty val) <> space <> done
  where kw = I.kindOf val
        done = "Undefined"

prettyResultFormula ((ctx,val):xs) = parens $
                                     "ite " <>
                                     pretty ctx <> space <>
                                     parens (pretty val) <> space <>
                                     pretty xs

instance Pretty i => Pretty (Maybe i) where
  pretty Nothing = ""
  pretty (Just a) = pretty a

instance Pretty S.Kind where pretty = pack . show
instance Pretty I.CV where pretty = pack . show
instance Pretty S.SBool where pretty = pack . show
instance Pretty [(Maybe VariantContext, I.CV)] where pretty = prettyResultFormula
instance Pretty ResultFormula where pretty (ResultFormula x) = pretty x

-- | newtype wrapper for better printing
newtype Result = Result (Result' Var) deriving (Eq,Generic)

instance Semigroup Result where (Result a) <> (Result b) = Result (a <> b)
deriving instance Monoid Result

instance Show Result where show = unpack . pretty

instance Pretty Result where pretty (Result r) = pretty r

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (a, b) = parens $ pretty a <> ", " <> pretty b

data Result' d = Result' { variables :: VariableMap d
                         , satResult :: Maybe VariantContext
                         } deriving (Eq, Show)

instance (Eq d, Hashable d) => Semigroup (Result' d) where
  (<>) Result' {variables=vl,satResult=sl} Result'{variables=vr,satResult=sr} =
    Result' { variables = vl <> vr
            , satResult = sl <> sr}

instance (Eq d, Hashable d) => Monoid (Result' d) where
  mempty = Result'{variables=mempty
                 ,satResult=mempty}
  mappend = (<>)

onVariables :: (VariableMap d -> VariableMap d) -> Result' d -> Result' d
onVariables f Result'{..} = Result'{variables=f variables, satResult}

onSatResult :: (Maybe VariantContext -> Maybe VariantContext) -> Result' d -> Result' d
onSatResult f Result'{..} = Result'{variables, satResult=f satResult}

insertToVariables :: Var -> ResultFormula -> Result' Var -> Result' Var
insertToVariables k v = onVariables (VariableMap . M.insertWith mappend k v . getVMap)

-- | O(1) insert a result prop into the result entry for special Sat variable
insertToSat :: (Ord d, IsString d) => Maybe VariantContext -> Result' d -> Result' d
insertToSat v = onSatResult (v `mappend`)

-- | check if the current context is sat or not
isSat :: C.Query Bool
isSat = do cs <- C.checkSat
           return $! case cs of
                       C.Sat -> True
                       _     -> False

-- | Generate a VSMT model
getVSMTModel :: (T.MonadQuery m, MonadIO m) => m S.SMTResult
getVSMTModel = T.getSMTResult

-- TODO: https://github.com/doyougnu/VSmt/issues/5
-- | Get a VSMT model in any supported monad.
getResult :: (MonadIO m, T.MonadQuery m) => Maybe VariantContext -> m Result
-- getResult Nothing =
getResult vf =
  do model <- getVSMTModel
     return $!
       Result $
       case model of
         m@(S.Satisfiable _ _)         ->
        -- when satisfiable we get the model dictionary, turn it into a
        -- result map and then insert the config proposition that created the
        -- satisfiable result into the __Sat element of the map
           toResMap . M.fromList . toList . S.getModelDictionary $! m
      -- (S.Unsatisfiable _ unsatCore) ->
        -- we apply f to True here because in the case of an unsat we want to
        -- save the proposition that produced the unsat, if we applied to
        -- false then we would have the negation of that proposition for
        -- unsat
        -- unSatToResult (f True) $ fromMaybe mempty unsatCore
         _                           -> mempty
 where
   toResMap m' =
     Result' {variables = VariableMap $ M.foldMapWithKey
              (\k a -> M.singleton (fromString k) (ResultFormula $ pure (vf, a))) m'
             ,satResult=vf}
