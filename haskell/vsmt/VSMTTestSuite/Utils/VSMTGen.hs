-----------------------------------------------------------------------------
-- |
-- Module    : Utils.VSMTGen
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module for writing QuickCheck/Smallcheck generators
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module Utils.VSMTGen where

import           Control.Monad         (liftM2, liftM3)
import           Data.Text             (Text, pack, singleton, toUpper)
import           GHC.Generics          (Generic)
import           Prelude               hiding (EQ, GT, LT)
import           Test.Tasty.QuickCheck

import           Core.Core
import           Core.Types

-------------------------- Newtype Wrappers ------------------------------------
newtype OnlyBools = OnlyBools { unOnlyBools :: Proposition }
  deriving (Generic,Show)

-------------------------- Helpers ---------------------------------------------
-- | Generate only alphabetical characters
genAlphaNum :: Gen Text
genAlphaNum = singleton <$> elements ['a'..'z']

genAlphaNumStr :: Gen Text
genAlphaNumStr = fmap mconcat $ flip suchThat (not . null) $ listOf genAlphaNum

genDim :: Gen Dim
genDim = Dim . toUpper <$> genAlphaNumStr

genSharedDim :: Gen Dim
genSharedDim = Dim . toUpper . pack <$> elements (zipWith (\a b -> [a,b]) ['a'..'d'] ['a'..'d'])

genSharedVar :: Gen Var
genSharedVar = singleton <$> elements ['a'..'j']

genVar :: Gen Var
genVar = genAlphaNumStr

genDouble :: Gen NPrim
genDouble = D <$> arbitrary

genInt :: Gen NPrim
genInt = I <$> arbitrarySizedIntegral

genPrim :: Gen NPrim
genPrim = oneof [genDouble, genInt]

genLit :: Gen Proposition
genLit = LitB <$> arbitrary

frequencies :: Gen Int
frequencies = elements [1..10]

-------------------------- Operator Generators ----------------------------------
genNN :: Gen N_N
genNN = elements [Neg]

genRefN :: Gen (ExRefType Var)
genRefN = do v <- genVar; elements [ExRefTypeI v, ExRefTypeD v]

genBB :: Gen B_B
genBB = elements [Not]

genNNN :: Gen NN_N
genNNN = elements [Add, Sub, Mult, Div]

genBBB :: Gen BB_B
genBBB = elements [Impl,Eqv, XOr, And, Or]

genNNB :: Gen NN_B
genNNB = elements [LT, LTE, GT, GTE, EQ, NEQ]


-------------------------- Prop Generators ----------------------------------
arbExpression :: Gen Dim -> Gen Var -> [Int] -> Int -> Gen NExpression
arbExpression _ _ _ 0 = RefI <$> genRefN
arbExpression genDm genVr ifreqs n = frequency $ zip ifreqs [ LitI <$> genPrim
                                                            , liftM2 OpI genNN l
                                                            , liftM3 OpII genNNN l l
                                                            , liftM3 ChcI genDim l l
                                                              ]
  where l = arbExpression genDm genVr ifreqs (n `div` 2)

arbProposition_ :: Gen Dim -> Gen Var -> ([Int], [Int]) -> Int -> Gen Proposition
arbProposition_ _ gv _ 0 = RefB <$> gv
arbProposition_ gd gv fs@(bfreqs, ifreqs) n = frequency $ zip bfreqs [ LitB <$> arbitrary
                                                                     , liftM2 OpB genBB l
                                                                     , liftM3 OpBB genBBB l l
                                                                     , liftM3 OpIB genNNB l' l'
                                                                     , liftM3 ChcB gd l l
                                                                     ]
  where l  = arbProposition_ gd gv fs (n `div` 2)
        l' = arbExpression   gd gv ifreqs (n `div` 2)

arbProposition :: Gen Dim -> Gen Var -> ([Int], [Int]) -> Int -> Gen Proposition
arbProposition = (((flip suchThat refsAreDisjoint .) . ) .) . arbProposition_

-- | newtype generator for propositional formulas of only booleans and no literals
onlyBoolProp :: Gen Dim -> Gen Var -> [Int] -> Int -> Gen Proposition
onlyBoolProp _ gv _ 0 = RefB <$> gv
onlyBoolProp gd gv bfreqs n = frequency $ zip bfreqs [ liftM2 OpB genBB l
                                                     , liftM3 OpBB genBBB l l
                                                     , liftM3 ChcB gd l l
                                                     ]
  where l  = onlyBoolProp gd gv bfreqs (n `div` 2)

------------------------------- Instances --------------------------------------
instance Arbitrary Var where arbitrary = genVar
instance Arbitrary Dim where arbitrary = genDim
instance Arbitrary NPrim where arbitrary = genPrim
instance Arbitrary N_N where arbitrary = genNN
instance Arbitrary (ExRefType Var) where arbitrary = genRefN
instance Arbitrary B_B  where arbitrary = genBB
instance Arbitrary NN_N where arbitrary = genNNN
instance Arbitrary BB_B where arbitrary = genBBB
instance Arbitrary NN_B where arbitrary = genNNB

instance Arbitrary Proposition where
  arbitrary = sized $ arbProposition genSharedDim genVar (repeat 3, repeat 3)
  shrink = genericShrink
  -- shrink (ChcB _ l r) = [l,r]
  -- shrink (OpIB _ l r) = [l,r]
  -- shrink (OpBB _ l r) = [l,r]
  -- shrink (OpB _ l) = pure l
  -- shrink x         = pure x

instance Arbitrary NExpression where
  arbitrary = sized $ arbExpression genSharedDim genVar (repeat 3)
  shrink = genericShrink

instance Arbitrary OnlyBools where
  arbitrary = fmap OnlyBools . sized $ onlyBoolProp genSharedDim genVar (repeat 3)
  shrink = genericShrink
