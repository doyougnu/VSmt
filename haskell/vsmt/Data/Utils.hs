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

-- import qualified Data.Map           as M
-- import qualified Data.SBV           as S

-- import           Data.SBV.Internals (cvToBool)
-- import           Data.Maybe         (isJust)
-- import           Data.String        (IsString(..))

-- import           Core.Core          (maxVariantCount)
-- import           SAT
import           Core.Types

-- genConfigPool :: Maybe VariantContext -> IO [Config]
genConfigPool :: a
genConfigPool = error "genconfig pool"
-- genConfigPool Nothing  = return mempty
-- genConfigPool (Just p) =
--   do
--     S.AllSatResult _ _ _ _ allRes <- S.allSat $ toPredicate p
--     let resMaps = S.getModelDictionary <$> allRes
--     return $ M.foldMapWithKey (\k a -> M.singleton (Dim $ fromString k) (cvToBool a)) <$> resMaps

vCoreMetrics :: a
vCoreMetrics = error "vcore metrics"

selectVariant :: a
selectVariant = error "select variant"


selectVariantTotal :: a
selectVariantTotal = error "select variant total"

renameDims :: (a -> b) -> Prop' a -> Prop' b
renameDims = fmap
