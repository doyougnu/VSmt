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

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.Core.Result where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Data              (Typeable)
import           Data.Hashable          (Hashable)
import           Data.Map               (toList)
import qualified Data.SBV.Control       as C
import qualified Data.SBV.Internals     as I
import qualified Data.SBV.Trans         as S
import qualified Data.SBV.Trans.Control as T
import           Data.Text              (Text)

import qualified Data.HashMap.Strict    as M
import           Data.String            (IsString, fromString)
import           GHC.Generics           (Generic)

import           Data.Core.Types

-- | An SMT Result formula, spine strict, we leave values lazy because they may
-- not be needed. We will be incrementally building these formulas through
-- conses and will not be doing any other real operations until the solver is
-- finished. Thus a list is an appropriate choice. We store the variantContext
-- with a value returned from SBV so that we can control the output to the user.
-- You should view this as an internal datatype, in the average case this will
-- be transformed into a Map of "variable" -> SMTLIB2 program where the SMTLIB2
-- program will dispatch the right value based on the values of dimensions
newtype ResultFormula a = ResultFormula [(VariantContext, a)]
    deriving (Eq,Ord,Show,Generic,Typeable,Semigroup,Monoid,Functor)

-- | We store the raw output from SBV (I.CV) to avoid having to use existentials
-- and to leverage the instance already made in the SBV library. This leads to
-- cleaner implementation on our end.
type VariableMap d = M.HashMap d (ResultFormula I.CV)

data Result d = Result { variables :: VariableMap d
                       , satResult :: VariantContext
                       }

instance (Eq d, Hashable d) => Semigroup (Result d) where
  (<>) Result {..} Result{variables=v,satResult=s} =
    Result { variables=variables <> v
           , satResult=satResult <> s}

instance (Eq d, Hashable d) => Monoid (Result d) where
  mempty = Result{variables=mempty
                 ,satResult=mempty}
  mappend = (<>)

onVariables :: (VariableMap d -> VariableMap d) -> Result d -> Result d
onVariables f Result{..} = Result{variables=f variables, satResult}

onSatResult :: (VariantContext -> VariantContext) -> Result d -> Result d
onSatResult f Result{..} = Result{variables, satResult=f satResult}

insertToVariables :: Var -> ResultFormula I.CV -> Result Var -> Result Var
insertToVariables k v = onVariables (M.insertWith mappend k v)

-- | O(1) insert a result prop into the result entry for special Sat variable
insertToSat :: (Ord d, IsString d) => VariantContext -> Result d -> Result d
insertToSat v = onSatResult (v `mappend`)

-- | check if the current context is sat or not
isSat :: C.Query Bool
isSat = do cs <- C.checkSat
           return $! case cs of
                       C.Sat -> True
                       _     -> False

test :: IO ()
test = S.runSMT $
  do x <- S.sInteger "x"
     y <- S.sDouble "y"
     C.query $
       do S.constrain $ x .> 4
          S.constrain $ y .< 4
          res <- getResult $ VariantContext $ RefB "foo"
          let res' = variables res
          C.io $ print $ res'

-- >>> test
-- fromList [("x",ResultFormula [(VariantContext {getVarFormula = RefB "foo"},5 :: Integer)]),("y",ResultFormula [(VariantContext {getVarFormula = RefB "foo"},5.0e-324 :: Double)])]

-- | Generate a VSMT model
getVSMTModel :: (T.MonadQuery m, MonadIO m) => m S.SMTResult
getVSMTModel = T.getSMTResult

-- | Get a VSMT model in any supported monad.
getResult :: (MonadIO m, T.MonadQuery m) => VariantContext -> m (Result Var)
getResult vf =
  do model <- getVSMTModel
     return $!
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
     Result {variables = M.foldMapWithKey
              (\k a -> M.singleton (fromString k) (ResultFormula $ pure (vf, a))) m'
            ,satResult=vf}
