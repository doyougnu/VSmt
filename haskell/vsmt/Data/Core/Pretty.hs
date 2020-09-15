-----------------------------------------------------------------------------
-- |
-- Module    : Data.Core.Pretty
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Pretty Printing for VSMT
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Data.Core.Pretty where

import qualified Data.Text       as Text
import           Prelude         hiding (EQ, GT, LT, log)

import           Data.Core.Types

class Pretty a where pretty :: a -> Text.Text

between :: Text.Text -> Text.Text -> Text.Text -> Text.Text
between sigilL a sigilR = sigilL <> a <> sigilR

parens :: Text.Text -> Text.Text
parens a = between "(" a ")"

newline :: Text.Text
newline = "\n"

space :: Text.Text
space = " "

instance Pretty a => Pretty (NExpr' a) where
  pretty (LitI (I i)) = Text.pack $ show i
  pretty (LitI (D d)) = Text.pack $ show d
  pretty (RefI b)     = pretty b
  pretty (OpI Neg e)  = "-" <> parens (pretty e)
  pretty (OpI Abs e)  = between "|" (pretty e) "|"
  pretty (OpI Sign e) = "signum" <> parens (pretty e)
  pretty (OpII o l r) = parens $ mconcat [pretty l, " ", pretty o, " ", pretty r]
  pretty (ChcI d l r) = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty a => Pretty (ExRefType a) where pretty (ExRefTypeI a) = pretty a
                                                pretty (ExRefTypeD a) = pretty a

instance Pretty NN_N where pretty Add  = ".+"
                           pretty Sub  = ".-"
                           pretty Mult = ".*"
                           pretty Div  = "./"
                           pretty Mod  = ".%"

instance Pretty NN_B where pretty LT  = ".<"
                           pretty LTE = ".<="
                           pretty GT  = ".>"
                           pretty GTE = ".>="
                           pretty EQ  = ".=="
                           pretty NEQ = ".!="


instance Pretty BB_B where pretty Impl = "impl"
                           pretty Eqv  = "iff"
                           pretty XOr  = "xor"
                           pretty And  = "and"
                           pretty Or   = "or"

instance Pretty Dim where pretty = Text.pack

instance Pretty a => Pretty (Prop' a) where
  pretty (LitB True)   = "#t"
  pretty (LitB False)  = "#f"
  pretty (RefB b)      = pretty b
  pretty (OpB Not e)   = "-" <> parens (pretty e)
  pretty (OpBB op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (OpIB op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (ChcB d l r)  = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty VariantContext where pretty = pretty . getVarFormula
instance Pretty Text.Text where pretty = id