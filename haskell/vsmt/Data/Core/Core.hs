-----------------------------------------------------------------------------
-- |
-- Module    : Data.Core.Core
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Internal functions for VSMT
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}

module Data.Core.Core where

import Data.Core.Types

----------------------------- Choice Manipulation ------------------------------
-- >> :t 'a'
class Configurable c b p where
  type MyConfig c
  type Found b :: * -> *
  retrieve  :: Dim -> Found b element
  configure :: MyConfig c -> p -> p

-- instance Configurable (Total Config) (Stmt a) where
--   configure conf (Assert p) = Assert $ configure conf p
--   configure conf (IfThenElse c t e) = IfThenElse (configure conf c)
--                                       (configure conf t)
--                                       (configure conf e)
--   configure _            x = x

-- instance Configurable (Total Config) (Prop a) where
--   configure (Total conf) (ChcB dim l r) | conf dim  = configure conf l
--                                         | otherwise = configure conf r
--   configure conf (OpB op a)    = OpB  op $ configure conf a
--   configure conf (OpBB op l r) = OpBB op (configure conf l) (configure conf r)
--   configure conf (OpIB op l r) = OpIB op (configure conf l) (configure conf r)
--   configure conf x = x

-- instance Configurable Config (Prop a) where
--   configure conf (ChcB dim l r) | conf dim = configure conf l
--                                         | otherwise = configure conf r
--   configure conf (OpB op a)    = OpB  op $ configure conf a
--   configure conf (OpBB op l r) = OpBB op (configure conf l) (configure conf r)
--   configure conf (OpIB op l r) = OpIB op (configure conf l) (configure conf r)
--   configure conf x = x

-- instance Configurable Config (NExpr a) where
--   configure conf (ChcI dim l r) | conf dim -> = configure conf l
--                                 | otherwise = configure conf r
--   configure conf (OpI op a)    = OpI  op $ configure conf a
--   configure conf (OpII op l r) = OpII op (configure conf l) (configure conf r)
--   configure conf x = x

-- configure :: Total Config -> Stmt a -> Plain (Stmt a)
-- configure conf (Assert p) = Plain . Assert $ configureProp conf p
-- configure conf (IfThenElse c t e) = Plain $
--                                     IfThenElse (configureProp conf p)
--                                     (configure conf t)
--                                     (configure conf e)
-- configure _            x = Plain x

-- configureProp :: Total Config -> Prop a -> Plain (Prop a)
-- configureProp conf (ChcB dim l r) | conf dim = configureProp conf l
--                                   | otherwise = configureProp conf r
-- configureProp conf (OpB op a)    = Plain . OpB  op $ configure conf a
-- configureProp conf (OpBB op l r) = Plain $ OpBB op (configure conf l) (configure conf r)
-- configureProp conf (OpIB op l r) = Plain $ OpIB op (configure conf l) (configure' conf r)
-- configureProp conf x = Plain x

-- selectVariantTotal tbs (ChcB t y n) =
--   case Map.lookup t tbs of
--     Just True  -> selectVariantTotal tbs y
--     Just False -> selectVariantTotal tbs n
--     Nothing    -> error "Under specified config supplied to selectVariantTotal function"
-- selectVariantTotal tb (OpB op x)    = OpB op $ selectVariantTotal tb x
-- selectVariantTotal tb (OpBB a l r)  = OpBB a
--                                       (selectVariantTotal tb l)
--                                       (selectVariantTotal tb r)
-- selectVariantTotal tb (OpIB op l r) = OpIB op
--                                       (selectVariantTotal' tb l)
--                                       (selectVariantTotal' tb r)
-- selectVariantTotal _  x             = x

-- selectVariantTotal' :: Ord d => Config d -> VIExpr d b -> VIExpr d b
-- selectVariantTotal' tb (ChcI t y n) =
--   case Map.lookup t tb of
--     Just True  -> selectVariantTotal' tb y
--     Just False -> selectVariantTotal' tb n
--     Nothing    -> error "Under specified config supplied to selectVariantTotal' function"
-- selectVariantTotal' tb (OpI op e)    = OpI op $ selectVariantTotal' tb e
-- selectVariantTotal' tb (OpII op l r) = OpII op (selectVariantTotal' tb l) (selectVariantTotal' tb r)
-- selectVariantTotal' _  x             = x


-- -- | Given a config and a Variational VProp term select the element out that the
-- -- config points to. In the edge case that a dimension is not found in a
-- -- configuration we simply recur through the choice but leave the choice intact.
-- -- This is required to avoid cases were the user wants to select a choice, but
-- -- that choice is guarded by a parent choice whose dimension is not in the
-- -- configuration
-- selectVariant :: Ord d => Config d -> VProp d a b -> VProp d a b
-- selectVariant tbs x@(ChcB t y n) = case Map.lookup t tbs of
--                                      Nothing    -> ChcB t (selectVariant tbs y) (selectVariant tbs n)
--                                      Just True  -> selectVariant tbs y
--                                      Just False -> selectVariant tbs n
-- selectVariant tb (OpB op x)    = OpB op $ selectVariant tb x
-- selectVariant tb (OpBB a l r)  = OpBB a (selectVariant tb l) (selectVariant tb r)
-- selectVariant tb (OpIB op l r) = OpIB op (selectVariant' tb l) (selectVariant' tb r)
-- selectVariant _  x             = x

-- selectVariant' :: Ord d => Config d -> VIExpr d b -> VIExpr d b
-- selectVariant' tb x@(ChcI t y n) = case Map.lookup t tb of
--                                      Nothing    ->ChcI t (selectVariant' tb y) (selectVariant' tb x)
--                                      Just True  -> selectVariant' tb y
--                                      Just False -> selectVariant' tb n
-- selectVariant' tb (OpI op e)    = OpI op $ selectVariant' tb e
-- selectVariant' tb (OpII op l r) = (OpII op) (selectVariant' tb l) (selectVariant' tb r)
-- selectVariant' _  x             = x
