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


-------------------------- Projections ------------------------------------------
-- | retrieve a set of all boolean references in a proposition
booleans :: Ord a => Prop' a -> Set.Set a
booleans (RefB b)     = Set.singleton b
booleans (OpB _ e)    = booleans e
booleans (OpBB _ l r) = booleans l `Set.union` booleans r
booleans (ChcB _ l r) = booleans l `Set.union` booleans r
booleans _            = Set.empty

-- | retrieve a set of all numeric references in a proposition
numerics :: Ord a => Prop' a -> Set.Set a
numerics (OpIB _ l r) = numerics' l `Set.union` numerics' r
numerics (OpB _ e)    = numerics e
numerics (OpBB _ l r) = numerics l `Set.union` numerics r
numerics (ChcB _ l r) = numerics l `Set.union` numerics r
numerics _            = Set.empty

numerics' :: Ord a => NExpr' a -> Set.Set a
numerics' (RefI (ExRefTypeI i)) = Set.singleton i
numerics' (RefI (ExRefTypeD d)) = Set.singleton d
numerics' (OpI _ e)             = numerics' e
numerics' (OpII _ l r)          = numerics' l `Set.union` numerics' r
numerics' (ChcI _ l r)          = numerics' l `Set.union` numerics' r
numerics' _                     = Set.empty

-- | Get the set of unique dimensions
dimensions :: Prop' a -> Set.Set Dim
dimensions (ChcB d l r) = Set.singleton d `Set.union`
                          dimensions l    `Set.union`
                          dimensions r
dimensions (OpIB _ l r) = dimensions' l `Set.union` dimensions' r
dimensions (OpB _ e)    = dimensions e
dimensions (OpBB _ l r) = dimensions l `Set.union` dimensions r
dimensions _            = Set.empty

dimensions' :: NExpr' a -> Set.Set Dim
dimensions' (ChcI d l r)          = Set.singleton d `Set.union`
                                    dimensions' l   `Set.union`
                                    dimensions' r
dimensions' (OpI _ e)             = dimensions' e
dimensions' (OpII _ l r)          = dimensions' l `Set.union` dimensions' r
dimensions' _                     = Set.empty

-- | given a proposition return how many possible variants the prop represents
posVariantCnt :: Prop' a -> Int
posVariantCnt = (2^) . Set.size . dimensions

-- | return the set of numeric variables with their type
numericsWithType :: Ord a => Prop' a -> Set.Set (ExRefType a)
numericsWithType (LitB _)     = Set.empty
numericsWithType (RefB _)     = Set.empty
numericsWithType (OpB _ e)    = numericsWithType e
numericsWithType (OpBB _ l r) = numericsWithType l `Set.union` numericsWithType r
numericsWithType (OpIB _ l r) = numericsWithType' l `Set.union` numericsWithType' r
numericsWithType (ChcB _ l r) = numericsWithType l `Set.union` numericsWithType r

numericsWithType' :: Ord a => NExpr' a -> Set.Set (ExRefType a)
numericsWithType' (LitI _)     = Set.empty
numericsWithType' (OpI _ e)    = numericsWithType' e
numericsWithType' (OpII _ l r) = numericsWithType' l `Set.union` numericsWithType' r
numericsWithType' (ChcI _ l r) = numericsWithType' l `Set.union` numericsWithType' r
numericsWithType' (RefI i)     = Set.singleton i

-- | return the set of integer variables
integers :: Ord a => Prop' a -> Set.Set a
integers = Set.map unbox . Set.filter isInt . numericsWithType
  where isInt (ExRefTypeI _) = True
        isInt (ExRefTypeD _) = False

        unbox (ExRefTypeI i) = i
        unbox (ExRefTypeD d) = d

-- | return the set of double variables
doubles :: Ord a => Prop' a -> Set.Set a
doubles p = (Set.\\) (numerics p) (integers p)

-------------------------- Predicates ------------------------------------------
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

isVariational :: Proposition -> Bool
isVariational = not . isPlain

-- | Propositions which only consist of mathematical relations
onlyRelations :: Proposition -> Bool
onlyRelations OpIB {} = True
onlyRelations _       = False

noDoubles :: Proposition -> Bool
noDoubles = Set.null . doubles

hasVariables :: Proposition -> Bool
hasVariables p = (not . Set.null $ booleans p) || (not . Set.null $ numerics p)

onlyBools :: Proposition -> Bool
onlyBools = Set.null . numerics
