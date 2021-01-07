-----------------------------------------------------------------------------
-- |
-- Module    : Core.Utils
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Core Utilities, we separate from the topmost Utils file because we may want
-- to hide these or not incite a cycle
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Core.Utils where

import           Control.DeepSeq (NFData)
import           Data.Bifunctor  (Bifunctor (..))
import           Data.Hashable   (Hashable)
import           GHC.Generics    (Generic)

-- | strict pairs
infix 2 :/\
data a :/\ b = !a :/\ !b
  deriving stock (Eq, Ord,Generic)

instance (NFData a, NFData b) => NFData ((:/\) a b)
instance (Hashable a, Hashable b) => Hashable ((:/\) a b)

sFst :: a :/\ b -> a
{-# INLINE sFst #-}
sFst (a :/\ _) = a

sSnd :: a :/\ b -> b
{-# INLINE sSnd #-}
sSnd (_ :/\ b) = b

instance (Show a, Show b) => Show ((:/\) a b) where
  show (a :/\ b) = show a ++ " :/\\ " ++ show b
instance Functor ((:/\) a) where fmap f (a :/\ b) = a :/\ f b
instance Bifunctor (:/\) where bimap f g (a :/\ b) = f a :/\ g b
