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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Core.Core where


import           Prelude                    hiding (EQ, GT, LT, log)
import qualified Data.Set as Set

import Data.Core.Types

----------------------------- Choice Manipulation ------------------------------
-- | Type class that dispatches the appropriate way to configure based on the
-- properties of the container storing dimensions
class Configurable conf p where
  configure :: conf -> p -> p

instance Configurable Config (Prop' a) where
  configure conf (ChcB d l r) | conf d  = configure conf l
                                | otherwise = configure conf r

  configure conf (OpB op a)    = OpB  op $ configure conf a
  configure conf (OpBB op l r) = OpBB op (configure conf l) (configure conf r)
  configure conf (OpIB op l r) = OpIB op (configure conf l) (configure conf r)
  configure _ x = x

instance Configurable Config (NExpr' a) where
  configure conf (ChcI d l r) | conf d  = configure conf l
                                | otherwise = configure conf r

  configure conf (OpI op a)    = OpI  op $ configure conf a
  configure conf (OpII op l r) = OpII op (configure conf l) (configure conf r)
  configure _ x = x

instance Configurable PartialConfig (Prop' a) where
  configure c@conf (ChcB d l r) =
    case conf d of
      Just b -> if b
                then configure c l
                else configure c r
      Nothing -> ChcB d (configure c l) (configure c r)

  configure conf (OpB op a)    = OpB  op $ configure conf a
  configure conf (OpBB op l r) = OpBB op (configure conf l) (configure conf r)
  configure conf (OpIB op l r) = OpIB op (configure conf l) (configure conf r)
  configure _ x = x

instance Configurable PartialConfig (NExpr' a) where
  configure c@conf (ChcI d l r) =
    case conf d of
      Just b -> if b
                then configure c l
                else configure c r
      Nothing -> ChcI d (configure c l) (configure c r)

  configure conf (OpI op a)    = OpI  op $ configure conf a
  configure conf (OpII op l r) = OpII op (configure conf l) (configure conf r)
  configure _ x = x


-------------------------- Predicates ------------------------------------------
-- | retrieve a set of all boolean references in a proposition
booleans :: Proposition -> Set.Set Var
booleans (RefB b)     = Set.singleton b
booleans (OpB _ e)    = booleans e
booleans (OpBB _ l r) = booleans l `Set.union` booleans r
booleans (ChcB _ l r) = booleans l `Set.union` booleans r
booleans _            = Set.empty

-- | retrieve a set of all numeric references in a proposition
numerics :: Proposition -> Set.Set Var
numerics (OpIB _ l r) = numerics' l `Set.union` numerics' r
numerics (OpB _ e)    = numerics e
numerics (OpBB _ l r) = numerics l `Set.union` numerics r
numerics (ChcB _ l r) = numerics l `Set.union` numerics r
numerics _            = Set.empty

numerics' :: NExpression -> Set.Set Var
numerics' (RefI (ExRefTypeI i)) = Set.singleton i
numerics' (RefI (ExRefTypeD d)) = Set.singleton d
numerics' (OpI _ e)             = numerics' e
numerics' (OpII _ l r)          = numerics' l `Set.union` numerics' r
numerics' (ChcI _ l r)          = numerics' l `Set.union` numerics' r
numerics' _                     = Set.empty

-- | False if there is a numeric variable with the same name as a boolean variable
refsAreDisjoint :: Proposition -> Bool
refsAreDisjoint prop = Set.null $ booleans prop `Set.intersection` numerics prop

-- | True if the proposition lacks choices
isPlain :: Proposition -> Bool
isPlain ChcB {}      = False
isPlain (OpB _ e)    = isPlain e
isPlain (OpBB _ l r) = isPlain l && isPlain r
isPlain (OpIB _ l r) = isPlain' l && isPlain' r
isPlain _            = True

isPlain' :: NExpression -> Bool
isPlain' ChcI {}      = False
isPlain' (OpI _ e)    = isPlain' e
isPlain' (OpII _ l r) = isPlain' l && isPlain' r
isPlain' _            = True
