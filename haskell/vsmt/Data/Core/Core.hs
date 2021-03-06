-----------------------------------------------------------------------------
-- |
-- Module    : Core.Core
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Internal functions for VSMT
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Core.Core where


import           Data.Foldable (toList)
import           Data.Function ((&))
import           Data.List     (foldl1')
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)
import qualified Data.Set      as Set
import           Prelude       hiding (EQ, GT, LT, log)

import           Core.Types


----------------------------- Choice Manipulation ------------------------------
-- | Type class that dispatches the appropriate way to configure based on the
-- properties of the container storing dimensions
class Configurable conf p where
  configure :: conf -> p -> p

instance Configurable c a => Configurable (Total c) a where
  configure (Total c) = configure c

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

-- | Retrieve the set of unique dimensions from numeric expressions
dimensions' :: NExpr' a -> Set.Set Dim
dimensions' (ChcI d l r)          = Set.singleton d `Set.union`
                                    dimensions' l   `Set.union`
                                    dimensions' r
dimensions' (OpI _ e)             = dimensions' e
dimensions' (OpII _ l r)          = dimensions' l `Set.union` dimensions' r
dimensions' _                     = Set.empty

-- | given a proposition return how many possible variants the prop represents
maxVariantCount :: Prop' a -> Int
maxVariantCount = (2^) . Set.size . dimensions

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

-- we use the list of lists format to allow counting as opposed to the function
-- type Config which does not allow counting
genConfigs :: Prop' a -> [Config]
genConfigs (toList . dimensions -> ds) = (Map.!) . Map.fromList <$> booleanCombinations ds
  where
    -- | given a list of stuff, generate a list of those things tagged with every
    -- possible boolean combination. i.e. booleanCombinations [1..3] = [[(1, True),
    -- (2, True) (3, True)], [(1, True), (2, True), (3, False)] ...]
    booleanCombinations :: [a] -> [[(a, Bool)]]
    booleanCombinations [] = []
    -- this singleton case is super important, if missed the fmap will only ever
    -- return empty list and the recursion won't consider the values
    booleanCombinations [x] = [[(x, True)], [(x, False)]]
    booleanCombinations (d:dims) =
      fmap ((d, True) :) cs ++ fmap ((d, False) :) cs
      where cs = booleanCombinations dims

-- | version of genConfigs that returns a map instead of a function
genConfigs' :: Prop' a -> [Map.Map Dim Bool]
genConfigs' (toList . dimensions -> ds) = Map.fromList <$> booleanCombinations ds
  where
    -- | given a list of stuff, generate a list of those things tagged with every
    -- possible boolean combination. i.e. booleanCombinations [1..3] = [[(1, True),
    -- (2, True) (3, True)], [(1, True), (2, True), (3, False)] ...]
    booleanCombinations :: [a] -> [[(a, Bool)]]
    booleanCombinations [] = []
    -- this singleton case is super important, if missed the fmap will only ever
    -- return empty list and the recursion won't consider the values
    booleanCombinations [x] = [[(x, True)], [(x, False)]]
    booleanCombinations (d:dims) =
      fmap ((d, True) :) cs ++ fmap ((d, False) :) cs
      where cs = booleanCombinations dims

-------------------------- Manipulation ------------------------------------------
configToContext :: Map.Map Dim Bool -> VariantContext
configToContext = Map.foldMapWithKey ((toVariantContext .) . go)
  where go k True  = bRef k
        go k False = bnot $ bRef k

-- | Alter the name of all dimensions by a function in a proposition
renameDims :: (Dim -> Dim) -> Prop' a -> Prop' a
renameDims f (ChcB d l r) = ChcB (f d) (renameDims f l)  (renameDims f r)
renameDims f (OpIB o l r) = OpIB   o   (renameDims' f l) (renameDims' f r)
renameDims f (OpB o e)    = OpB    o   (renameDims f e)
renameDims f (OpBB o l r) = OpBB   o   (renameDims f l)  (renameDims f r)
renameDims _ x            = x

-- | Alter the name of all dimensions by a function in a numeric proposition
renameDims' :: (Dim -> Dim) -> NExpr' a -> NExpr' a
renameDims' f (ChcI d l r) = ChcI (f d) (renameDims' f l) (renameDims' f r)
renameDims' f (OpII o l r) = OpII   o   (renameDims' f l) (renameDims' f r)
renameDims' f (OpI o e)    = OpI    o   (renameDims' f e)
renameDims' _ x            = x

-------------------------- Diagnostics ------------------------------------------
compressionRatio :: (Configurable Config p, p ~ Prop' a) => p -> Float
compressionRatio prop
  | total == 0 = 0
  | otherwise = fromIntegral numerator / fromIntegral total
  where numerator = length $ toList prop
        configs = genConfigs prop
        total = sum $ length . toList . flip configure prop <$> configs

-- | Count all the choiceCount in a proposition
choiceCount :: Prop' a -> Int
choiceCount (ChcB _ l r) = 1 + choiceCount l + choiceCount r
choiceCount (OpIB _ l r) = choiceCount' l + choiceCount' r
choiceCount (OpB _ e)    = choiceCount e
choiceCount (OpBB _ l r) = choiceCount l + choiceCount r
choiceCount _            = 0

choiceCount' :: NExpr' a -> Int
choiceCount' (ChcI _ l r) = 1 + choiceCount' l + choiceCount' r
choiceCount' (OpII _ l r) = choiceCount' l + choiceCount' r
choiceCount' (OpI _ e)    = choiceCount' e
choiceCount' _            = 0

-- | Count all the choiceCount in a proposition
plainCount :: Prop' a -> Int
plainCount (ChcB _ l r) = choiceCount l + choiceCount r
plainCount (OpIB _ l r) = choiceCount' l + choiceCount' r
plainCount (OpB _ e)    = choiceCount e
plainCount (OpBB _ l r) = choiceCount l + choiceCount r
plainCount _            = 1

plainCount' :: NExpr' a -> Int
plainCount' (ChcI _ l r) = choiceCount' l + choiceCount' r
plainCount' (OpII _ l r) = choiceCount' l + choiceCount' r
plainCount' (OpI _ e)    = choiceCount' e
plainCount' _            = 1

dimensionCount :: Prop' a -> Int
dimensionCount = Set.size . dimensions

-------------------------- Predicates ------------------------------------------
-- | False if there is a numeric variable with the same name as a boolean variable
refsAreDisjoint :: Proposition -> Bool
refsAreDisjoint prop = Set.null $ booleans prop `Set.intersection` numerics prop

-- | True if the proposition lacks choices
validatePlain :: Proposition -> Maybe (Plain Proposition)
validatePlain p | choiceCount p == 0 = Just $ Plain p
                | otherwise          = Nothing

validateTotal :: PartialConfig -> Prop' a -> Maybe (Total Config)
validateTotal c (toList . dimensions -> ds) =
  sequence (c <$> ds) & \case Just _ -> Just $ Total $! fromJust <$> c
                              _      -> Nothing

isVariational :: Proposition -> Bool
isVariational p = validatePlain p & \case Nothing -> True
                                          _       -> False

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

noNestedChoices :: Proposition -> Bool
noNestedChoices = go True
  where go True (ChcB _ l r) = go False l && go False r
        go False ChcB{}      = False
        go b (OpIB _ l r) = go' b l && go' b r
        go b (OpB _ e)    = go b e
        go b (OpBB _ l r) = go b l && go b r
        go _ _            = True

        go' True (ChcI _ l r) = go' False l && go' False r
        go' False ChcI {}     = False
        go' b (OpII _ l r)    = go' b l && go' b r
        go' b (OpI _ e)       = go' b e
        go' _ _               = True

-------------------------- Construction -----------------------------------------
fromList :: Foldable f => (a -> a -> a) -> f a -> a
fromList _ (null -> True) = error "Empty Foldable in fromList'"
fromList f (toList -> xs) = foldl1' f xs

conjoin :: (Boolean b, Foldable f) => f b -> b
conjoin = fromList (&&&)

disjoin :: (Boolean b, Foldable f) => f b -> b
disjoin = fromList (|||)
