{-# LANGUAGE LambdaCase #-}
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Core.Result where

import           Data.Data        (Typeable)
import           Data.Text        (Text)
import qualified Data.SBV         as S
import qualified Data.SBV.Control as C
import qualified Data.SBV.Internals as I
import qualified Data.SBV.Trans.Control as T
import           Control.Monad.IO.Class (MonadIO)

import           GHC.Generics     (Generic)
import qualified Data.Map.Strict as M
import           Data.String (fromString, IsString())

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
type VariableMap d = M.Map d (ResultFormula I.CV)

data Result d = Result { variables :: VariableMap d
                       , satResult :: VariantContext
                       }

instance Ord d => Semigroup (Result d) where
  (<>) Result {..} Result{variables=v,satResult=s} =
    Result { variables=variables <> v
           , satResult=satResult <> s}

instance Ord d => Monoid (Result d) where
  mempty = Result{variables=mempty
                 ,satResult=mempty}
  mappend = (<>)

onVariables :: (VariableMap d -> VariableMap d) -> Result d -> Result d
onVariables f Result{..} = Result{variables=f variables, satResult}

onSatResult :: (VariantContext -> VariantContext) -> Result d -> Result d
onSatResult f Result{..} = Result{variables, satResult=f satResult}

insertToVariables :: Ord d => d -> ResultFormula I.CV -> Result d -> Result d
insertToVariables k v = onVariables (M.insertWith mappend k v)

-- | O(1) insert a result prop into the result entry for special Sat variable
insertToSat :: (Ord d, IsString d) => VariantContext -> Result d -> Result d
insertToSat v = onSatResult (v `mappend`)

-- | check if the current context is sat or not
isSat :: C.Query Bool
isSat = do cs <- C.checkSat
           return $! case cs of
                       C.Sat -> True
                       _   -> False

-- getResult :: Resultable d => ResultProp d -> Query (Result d)
-- getResult = getResultWith . dispatchProp
-- {-# INLINE getResult #-}

test :: IO ()
test = S.runSMT $
  do x <- S.sInteger "x"
     y <- S.sDouble "y"
     C.query $
       do S.constrain $ x .> 4
          S.constrain $ y .< 4
          res <- C.getSMTResult
          C.io $ print $ S.getModelDictionary res

test2 :: IO ()
test2 = S.runSMT $
  do x <- S.sInteger "x"
     y <- S.sDouble "y"
     C.query $
       do S.constrain $ x .> 4
          S.constrain $ y .< 4
          res <- getResult $ VariantContext $ RefB "foo"
          let res' = variables res
          C.io $ print $ res'

-- >>> test
-- fromList [("x",5 :: Integer),("y",5.0e-324 :: Double)]

-- >>> test2
-- fromList [("x",ResultFormula [(VariantContext {getVarFormula = RefB "foo"},5 :: Integer)]),("y",ResultFormula [(VariantContext {getVarFormula = RefB "foo"},5.0e-324 :: Double)])]

-- >>> :t S.ite true (S.sInt8 "x") (S.sInt8 "y")
-- <interactive>:1:1-38: error:
--     * No instance for (S.Mergeable (S.Symbolic S.SInt8))
--         arising from a use of `S.ite'
--     * In the expression: S.ite true (S.sInt8 "x") (S.sInt8 "y")
-- prep :: S.Symbolic (S.SBool, S.SBool, S.SInteger)
-- prep = do result <- S.sInteger "result"
--           a <- S.sBool "a"
--           b <- S.sBool "b"
--           return (a, b, result)


-- t1 :: (S.SBool, S.SBool, S.SInteger) -> S.Symbolic (S.SBool, S.SBool, S.SInteger)
-- t1 x@(a, b, result) = S.ite (a &&& b) (result .== 1) false

-- t2 :: (S.SBool, S.SBool, S.SInteger) -> S.Symbolic (S.SBool, S.SBool, S.SInteger)
-- t2 x@(a,b,result) = S.ite (a &&& (bnot b)) (result .== 2)

-- question :: (S.SBool, S.SBool, S.SInteger) -> S.Symbolic ()
-- question x@(a,b,result) = return () -- S.constrain $ (result .> 0) &&& (result .< 5)

-- >>> S.allSat $ prep >>= t3 >>= question
-- <interactive>:9:2-9: error:
--     Not in scope: `S.allSat'
--     No module named `S' is imported.

-- | a mapping keeping the config and the unsatisfiable core of the config; used
-- in the case of an usat result
-- newtype UnSatResult d = UnSatResult (M.Map (ResultProp d) UnSatCore)
--                       deriving (Eq,Show,Generic,Semigroup,Monoid)

-- newtype Result d = Result (ResultMap d, UnSatResult d)
--                  deriving (Eq,Show,Generic,Semigroup,Monoid)


getVSMTModel :: (T.MonadQuery m, MonadIO m) => m S.SMTResult
getVSMTModel = T.getSMTResult

getResult :: (MonadIO m, T.MonadQuery m, Ord d, Show d, IsString d) =>
  VariantContext -> m (Result d)
getResult vf =
  do model <- getVSMTModel
     return $!
       case model of
         m@(S.Satisfiable _ _)         ->
        -- when satisfiable we get the model dictionary, turn it into a
        -- result map and then insert the config proposition that created the
        -- satisfiable result into the __Sat element of the map
           toResMap . S.getModelDictionary $! m
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
