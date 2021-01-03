-----------------------------------------------------------------------------
-- |
-- Module    : Core.Pretty
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Pretty Printing for VSMT
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Core.Pretty where

import qualified Data.Text       as Text
import           Prelude         hiding (EQ, GT, LT, log)
import qualified Data.HashMap.Strict as M

import           Core.Types

class Pretty a where pretty :: a -> Text.Text

between :: Text.Text -> Text.Text -> Text.Text -> Text.Text
between !sigilL !a !sigilR = sigilL <> a <> sigilR

parens :: Text.Text -> Text.Text
parens a = between "(" a ")"

newline :: Text.Text
newline = "\n"

space :: Text.Text
space = " "

comma :: Text.Text
comma = ","

bar :: Text.Text
bar = Text.replicate 80 "-"

seperator :: Text.Text
seperator = newline <> bar <> newline <> bar <> newline

instance Pretty a => Pretty (NExpr' a) where
  pretty (LitI (I i)) = Text.pack $ show i
  pretty (LitI (D d)) = Text.pack $ show d
  pretty (RefI b)     = pretty b
  pretty (OpI o x@(RefI _))  = pretty o <> pretty x
  pretty (OpI Abs e)  = between "|" (pretty e) "|"
  pretty (OpI o e)  = pretty o <> parens (pretty e)
  pretty (OpII o l r) = parens $ mconcat [pretty l, " ", pretty o, " ", pretty r]
  pretty (ChcI d l r) = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty a => Pretty (ExRefType a) where pretty (ExRefTypeI a) = pretty a
                                                pretty (ExRefTypeD a) = pretty a

instance Pretty Char where pretty = Text.singleton
instance Pretty String where pretty = Text.pack
instance Pretty Dim where pretty = getDim

instance Pretty N_N where pretty Neg  = "-"
                          pretty Sign = "signum "
                          pretty Abs  = "||"

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

instance Pretty B_B where pretty Not = "~"

instance Pretty BB_B where pretty Impl = "impl"
                           pretty Eqv  = "iff"
                           pretty XOr  = "xor"
                           pretty And  = "and"
                           pretty Or   = "or"

instance Pretty a => Pretty (Prop' a) where
  pretty (LitB True)   = "#t"
  pretty (LitB False)  = "#f"
  pretty (RefB b)      = pretty b
  pretty (OpB Not r@(RefB _))   = "~" <> pretty r
  pretty (OpB o e)   = pretty o <> parens (pretty e)
  pretty (OpBB op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (OpIB op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (ChcB d l r)  = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty CheckableResult where
  pretty c = seperator <> pretty m <> seperator
    where m = getChkResult c

instance Pretty Value where pretty (N (I i)) = Text.pack $ show i
                            pretty (N (D d)) = Text.pack $ show d
                            pretty (B b)     = Text.pack $ show b

instance Pretty VariantContext where pretty = pretty . getVarFormula
instance Pretty Text.Text where pretty = id
instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = parens $ pretty a <> space <> comma <> space <> pretty b

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = mempty
  pretty (Just a) = pretty a

instance Pretty (M.HashMap Var [(Maybe VariantContext, Value)]) where
  pretty =
    M.foldMapWithKey (\k v -> pretty k <> "   -->   " <> pretty v <> newline)

instance Pretty [(Maybe VariantContext, Value)] where
  pretty [] = mempty
  pretty [(Nothing, val)] = pretty val
  pretty [(Just ctx, val)]    = parens $
    "ite"
    <> space
    <> pretty ctx
    <> space
    <> parens (pretty val)
    <> space
    <> done
    where done = "Undefined"
  pretty ((Nothing, val):xs) = "ite" <> space <> parens "True :: Bool" <> space
                               <> parens (pretty val) <> space <> pretty xs
  pretty ((Just ctx, val):xs) = "ite" <> space <> pretty ctx <> space
                               <> parens (pretty val) <> space <> pretty xs

-- instance {-# OVERLAPS #-} Pretty a => Pretty [a] where
--   pretty [] = mempty
--   pretty xs = mconcat $ pretty <$> xs
