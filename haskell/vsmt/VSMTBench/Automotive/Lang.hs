{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}

module Lang where

import           Control.DeepSeq    (NFData)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.List          (delete)
import           Data.Monoid        ((<>))
import           GHC.Generics
import           Data.Text

import           Core.Core          (fromList)
import qualified Core.Types         as V
import           Debug.Trace

data AutoLang = AutoLit Bool
              | AutoRef Text
              | Ctx RBOp ALang AutoLang
              | AutoNot AutoLang
              | BBinary BOp AutoLang AutoLang
              | RBinary RBOp ALang ALang
                deriving (Eq, Ord, Generic)

data BOp = And | Or | Impl | Eqv | Xor deriving (Eq, Ord,Generic)
data RBOp = LST | EQL | GRT | LSTE  | NEQL | GRTE  deriving (Eq,Ord,Generic)

data ALang = ALit Integer
           | AVar Text
           | ACtx ALang
           | Neg ALang
           | ABinary AOp ALang ALang
           deriving (Eq, Ord, Generic)

data AOp = Add | Subtract | Multiply | Divide | Modulus deriving (Eq, Ord,Generic)

prettyAuto :: AutoLang -> String
prettyAuto = top
  where
    top :: AutoLang -> String
    top (BBinary b l r)  = mconcat [sub l, " ", show b, " ", sub r]
    top (RBinary nb l r) = mconcat [prettyAuto' l, " ", show nb, " ", prettyAuto' r]
    top (AutoNot r)      = mconcat ["¬", prettyAuto r]
    top (Ctx rb al rs)   = mconcat ["Ctx", show rb," ", prettyAuto' al," ", prettyAuto rs]
    top e                = sub e

    sub :: AutoLang -> String
    sub (AutoLit b) = if b then "#T" else "#F"
    sub (AutoRef a) = show a
    sub e           = "(" ++ top e ++ ")"

prettyAuto' :: ALang -> String
prettyAuto' (ALit i) = show i
prettyAuto' (ACtx expr) = mconcat ["ACtx: ", show expr]
prettyAuto' (AVar a) = show a
prettyAuto' (Neg a)  = mconcat ["−", prettyAuto' a]
prettyAuto' (ABinary o l r) = mconcat [prettyAuto' l, " ", show o, " ", prettyAuto' r]


instance Show AOp where show Add      = "+"
                        show Subtract = "-"
                        show Multiply = "*"
                        show Divide   = "/"
                        show Modulus  = "%"

instance Show RBOp where show LST  = "<"
                         show LSTE = "≤"
                         show GRT  = ">"
                         -- show GRTE = "≥"
                         show GRTE = ">="
                         show EQL  = "=="
                         show NEQL = "≠"

instance Show BOp where show Impl = "→"
                        show Eqv  = "↔"
                        show Xor  = "⊻"
                        show And  = "∧"
                        show Or   = "∨"

instance Show AutoLang where show = prettyAuto
instance Show ALang where show = prettyAuto'
instance NFData AutoLang
instance NFData ALang
instance NFData AOp
instance NFData RBOp
instance NFData BOp

atMost1 :: [AutoLang] -> AutoLang
atMost1 [] = error "empty list on input of atMost1"
atMost1 [x] = x
atMost1 xs = cs V.&&& fromList (V.&&&) disjuncs
  where disjuncs = [(V.bnot x V.||| V.bnot y) | (x, i) <- labeled
                                        , (y, j) <- labeled
                                        , i < j
                                        ]

        labeled = Prelude.zip xs [1..]
        cs = fromList (V.|||) xs

instance V.Boolean AutoLang where
  true  = AutoLit True
  false = AutoLit False
  bnot  = AutoNot
  (&&&) = BBinary And
  (|||) = BBinary Or
  (<+>) = BBinary Xor
  (==>) = BBinary Impl
  (<=>) = BBinary Eqv

instance V.Prim AutoLang ALang where
  (.<)  = RBinary LST
  (.<=) = RBinary LSTE
  (.==) = RBinary EQL
  (./=) = RBinary NEQL
  (.>=) = RBinary GRTE
  (.>)  = RBinary GRT

instance V.PrimN ALang where
  (./) = ABinary Divide
  (.%) = ABinary Modulus

instance Num ALang where
  fromInteger = ALit
  (+) = ABinary Add
  (*) = ABinary Multiply
  (-) = ABinary Subtract
  negate = Neg
  signum = error "signum not supported in AutoLang"
  abs    = error "absolute value not supported in AutoLang"
