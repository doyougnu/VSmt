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
