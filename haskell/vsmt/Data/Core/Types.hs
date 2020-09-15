-----------------------------------------------------------------------------
-- |
-- Module    : Data.Core.Types
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Internal Types for the vsmt library
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Core.Types where

import           Control.DeepSeq (NFData)
import           Control.Monad   (liftM2)
import           Data.Fixed      (mod')
import qualified Data.SBV.Trans  as S
import qualified Data.Sequence   as Seq
import           Data.String     (IsString)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Prelude         hiding (EQ, GT, LT, lookup)


-- | A feature is a named, boolean configuration option.
type Var = Text
-- newtype Dim = Dim Text deriving (Eq,Ord,NFData,Hashable)
type Dim = String

type Config  = Dim -> Bool
type PartialConfig = Dim -> Maybe Bool

-- | empty type to represent when an 'a' is total
newtype Total a = Total a

-- | an type to represent that an 'a' is plain
newtype Plain a = Plain a

--
-- * Syntax
--

-- | A Variant formula is a possibly smt propositional formula with references
-- restricted to dimensions only. We could choose to force that the dimensions
-- \in prop they describe but this artificially restricts the expressiveness of
-- the system and is best left to the end-user
newtype VariantContext = VariantContext { getVarFormula :: Prop' Dim }
  deriving (Eq,Generic,Ord,Show)

-- | An SMT Program is a sequence of statements interacting with the base solver
type Prog = Seq.Seq
type Proposition = Prop' Var
type NExpression = NExpr' Var
type SMTProg = Prog (Stmt Proposition)

instance Semigroup VariantContext where
  (<>) (getVarFormula -> x) (getVarFormula -> y) = VariantContext (OpBB Or x y)

-- | Top level Syntax
data Stmt a = Assert !a         -- ^ constraint the prop
            | Call SolverOp            -- ^ side effectual interact with the solver
            | IfThenElse !a (Stmt a) (Stmt a)
            | Define Var Type
            | StoreCore
            deriving (Eq,Generic,Ord,Functor,Traversable,Foldable)

data SolverOp = IsSat      -- ^ call sat
              | SatModel   -- ^ sat with model
              | Optimize   -- ^ optimize
              | Prove      -- ^ Prove, this could just be sat $ negate prop though
              | CodeGen    -- ^ generate code
              deriving (Eq,Ord,Show)

-- | Boolean expressions with choices, value and spine strict
-- TODO combine using GADTS
data Prop' a
   = LitB Bool                           -- ^ boolean literals
   | RefB !a                             -- ^ Bool References
   | OpB  B_B  !(Prop' a)                -- ^ Unary Operators
   | OpBB BB_B !(Prop' a)  !(Prop' a)    -- ^ Binary Operators
   | OpIB NN_B !(NExpr' a) !(NExpr' a)   -- ^ SMT Arithmetic
   -- we leave choices to be lazy for performance in selection/configuration. It
   -- may be the case that one alternative is never selected for
   | ChcB !Dim (Prop' a) (Prop' a)       -- ^ Choices
  deriving (Eq,Generic,Show,Functor,Traversable,Foldable,Ord)

-- | Numerical Expressions with Choices
data NExpr' a
  = LitI NPrim                        -- ^ Arithmetic Literals
  | RefI !(ExRefType a)               -- ^ Arithmetic References
  | OpI  N_N  !(NExpr' a)             -- ^ Unary Operators
  | OpII NN_N !(NExpr' a) !(NExpr' a) -- ^ Binary Operators
  | ChcI !Dim  (NExpr' a) (NExpr' a)  -- ^ SMT Choices
  deriving (Eq,Generic,Show,Functor,Traversable,Foldable,Ord)

-- | Types of references
data Type = TBool
          | TInt
          | TDouble
          deriving (Eq,Generic,Show,Ord)

-- | Mirroring NPrim with Symbolic types for the solver
data SymNum = SyI !S.SInt64
            | SyD !S.SDouble
          deriving (Eq, Generic,Show)

data ExRefType a = ExRefTypeI a | ExRefTypeD a
  deriving (Eq,Generic,Show,Ord,Functor,Traversable,Foldable)

-- | data constructor for Numeric operations
data NPrim = I !Integer | D {-# UNPACK #-} !Double
  deriving (Eq,Generic,Ord,Show)

-- | Unary Numeric Operator
data N_N = Neg | Abs | Sign deriving (Eq,Generic,Ord,Show)

-- | Binary Boolean operators
data B_B = Not deriving (Eq,Generic,Ord,Show)

-- | Binary Numeric Operators
data NN_N = Add | Sub | Mult | Div | Mod deriving (Eq,Generic,Ord,Show)

-- | Binary Boolean operators
data BB_B = And | Or | Impl | Eqv | XOr deriving (Eq,Generic,Ord,Show)

-- | Binary Numeric predicate operators
data NN_B = LT | LTE | GT | GTE | EQ | NEQ deriving (Eq,Generic,Ord,Show)

-- | add div and mod to num
class Num n => PrimN n where
  (./), (.%) :: n -> n -> n

-- | Overload the primitive operators
class (Boolean b, PrimN n) => Prim b n where
  (.<), (.<=), (.==), (./=), (.>=), (.>) :: n -> n -> b

infix 4 .<, .<=, .==, ./=, .>=, .>
infixl 7 ./, .%

-- | some not so smart constructors, pinning a to string because we will be
-- using String the most
iRef :: IsString a => a -> NExpr' a
iRef = RefI . ExRefTypeI
{-# INLINE iRef #-}

dRef :: IsString a => a -> NExpr' a
dRef = RefI . ExRefTypeD
{-# INLINE dRef #-}

bRef :: IsString a => a -> Prop' a
bRef = RefB
{-# INLINE bRef #-}

iLit :: Integer -> NExpr' a
iLit = LitI . I
{-# INLINE iLit #-}

dLit :: Double -> NExpr' a
dLit = LitI . D
{-# INLINE dLit #-}

bChc :: String -> Prop' a -> Prop' a -> Prop' a
bChc = ChcB
{-# INLINE bChc #-}

iChc :: String -> NExpr' a -> NExpr' a -> NExpr' a
iChc = ChcI
{-# INLINE iChc #-}

-- dim :: String -> Dim
-- dim = Dim . pack

-- dimReference :: String -> Prop' Dim
-- dimReference = RefB . dim

-- dimToString :: Dim -> String
-- dimToString (Dim s) = unpack s

-- | Begin primitive instances

instance PrimN Integer where
  (./) = div
  (.%) = mod

instance PrimN Double where
  (./) = (/)
  (.%) = mod'

instance Prim Bool Integer where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)

instance Prim Bool Double where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)

instance Prim (Prop' a) Integer where
  (.<)  i j = OpIB LT  (LitI $ I i) (LitI $ I j)
  (.<=) i j = OpIB LTE (LitI $ I i) (LitI $ I j)
  (.==) i j = OpIB EQ  (LitI $ I i) (LitI $ I j)
  (./=) i j = OpIB NEQ (LitI $ I i) (LitI $ I j)
  (.>=) i j = OpIB GTE (LitI $ I i) (LitI $ I j)
  (.>)  i j = OpIB GT  (LitI $ I i) (LitI $ I j)

instance Prim (Prop' a) Double where
  (.<)  i j = OpIB LT  (LitI $ D i) (LitI $ D j)
  (.<=) i j = OpIB LTE (LitI $ D i) (LitI $ D j)
  (.==) i j = OpIB EQ  (LitI $ D i) (LitI $ D j)
  (./=) i j = OpIB NEQ (LitI $ D i) (LitI $ D j)
  (.>=) i j = OpIB GTE (LitI $ D i) (LitI $ D j)
  (.>)  i j = OpIB GT  (LitI $ D i) (LitI $ D j)

-- * SBV instances

-- | we'll need to mirror the NPrim data type in SBV via SymNum
instance Num SymNum where
  fromInteger = SyI . S.literal . fromInteger

  abs (SyI i) = SyI $ abs i
  abs (SyD d) = SyD $ S.fpAbs d

  negate (SyI i) = SyI $ negate i
  negate (SyD d) = SyD $ S.fpNeg d

  signum (SyI i) = SyI $ signum i
  signum (SyD d) = SyD $ signum (S.fromSDouble S.sRoundNearestTiesToAway d)

  (SyI i) + (SyI i') = SyI $ i + i'
  (SyD d) + (SyI i)  = SyD $ d + S.sFromIntegral i
  (SyI i) + (SyD d)  = SyD $ S.sFromIntegral i + d
  (SyD d) + (SyD d') = SyD $ d + d'

  (SyI i) - (SyI i') = SyI $ i - i'
  (SyD d) - (SyI i)  = SyD $ d - S.sFromIntegral i
  (SyI i) - (SyD d)  = SyD $ S.sFromIntegral i - d
  (SyD d) - (SyD d') = SyD $ d - d'

  (SyI i) * (SyI i') = SyI $ i * i'
  (SyD d) * (SyI i)  = SyD $ d * S.sFromIntegral i
  (SyI i) * (SyD d)  = SyD $ d * S.sFromIntegral i
  (SyD d) * (SyD d') = SyD $ d * d'

instance PrimN SymNum where
  (SyI i) ./ (SyI i') = SyI $ i ./ i'
  (SyD d) ./ (SyI i)  = SyD $ d ./ S.sFromIntegral i
  (SyI i) ./ (SyD d)  = SyD $ S.sFromIntegral i ./ d
  (SyD d) ./ (SyD d') = SyD $ d ./ d'


  (SyI i) .% (SyI i') = SyI $ i .% i'
  (SyD d) .% (SyI i)  = SyI $ S.fromSDouble S.sRoundNearestTiesToAway d .% i
  (SyI i) .% (SyD d)  = SyI $ i .% S.fromSDouble S.sRoundNearestTiesToAway d
  (SyD d) .% (SyD d') = SyD $ S.fpRem d d'


instance PrimN S.SDouble where
  (./)  = S.fpDiv S.sRoundNearestTiesToAway
  (.%)  = undefined

instance PrimN S.SInt8 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt16 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt32 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt64 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInteger where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance S.Mergeable SymNum where
  symbolicMerge _ b thn els
    | Just result <- S.unliteral b = if result then thn else els
    | otherwise = els

instance S.EqSymbolic SymNum where
  (.==) (SyI i) (SyI i') = (S..==) i i'
  (.==) (SyD d) (SyI i') = (S..==) d (S.sFromIntegral i')
  (.==) (SyI i) (SyD d)  = (S..==) (S.sFromIntegral i) d
  (.==) (SyD d) (SyD d') = (S..==) d d'

  (./=) (SyI i) (SyI i') = (S../=) i i'
  (./=) (SyD d) (SyI i') = (S../=) d (S.sFromIntegral i')
  (./=) (SyI i) (SyD d)  = (S../=) (S.sFromIntegral i) d
  (./=) (SyD d) (SyD d') = (S../=) d d'

instance S.OrdSymbolic SymNum where
  (.<) (SyI i) (SyI i') = (S..<) i i'
  (.<) (SyD d) (SyI i)  = (S..<) d (S.sFromIntegral i)
  (.<) (SyI i) (SyD d)  = (S..<) (S.sFromIntegral i) d
  (.<) (SyD d) (SyD d') = (S..<) d d'

  (.<=) (SyI i) (SyI i') = (S..<=) i i'
  (.<=) (SyD d) (SyI i)  = (S..<=) d (S.sFromIntegral i)
  (.<=) (SyI i) (SyD d)  = (S..<=) (S.sFromIntegral i) d
  (.<=) (SyD d) (SyD d') = (S..<=) d d'

  (.>=) (SyI i) (SyI i') = (S..>=) i i'
  (.>=) (SyD d) (SyI i)  = (S..>=) d (S.sFromIntegral i)
  (.>=) (SyI i) (SyD d)  = (S..>=) (S.sFromIntegral i) d
  (.>=) (SyD d) (SyD d') = (S..>=) d d'

  (.>) (SyI i) (SyI i') = (S..>) i i'
  (.>) (SyD d) (SyI i)  = (S..>) d (S.sFromIntegral i)
  (.>) (SyI i) (SyD d)  = (S..>) (S.sFromIntegral i) d
  (.>) (SyD d) (SyD d') = (S..>) d d'

instance Prim S.SBool SymNum where
  (.<) (SyI i) (SyI i') = (S..<) i i'
  (.<) (SyD d) (SyI i)  = (S..<) d (S.sFromIntegral i)
  (.<) (SyI i) (SyD d)  = (S..<) (S.sFromIntegral i) d
  (.<) (SyD d) (SyD d') = (S..<) d d'

  (.<=) (SyI i) (SyI i') = (S..<=) i i'
  (.<=) (SyD d) (SyI i)  = (S..<=) d (S.sFromIntegral i)
  (.<=) (SyI i) (SyD d)  = (S..<=) (S.sFromIntegral i) d
  (.<=) (SyD d) (SyD d') = (S..<=) d d'

  (.>=) (SyI i) (SyI i') = (S..>=) i i'
  (.>=) (SyD d) (SyI i)  = (S..>=) d (S.sFromIntegral i)
  (.>=) (SyI i) (SyD d)  = (S..>=) (S.sFromIntegral i) d
  (.>=) (SyD d) (SyD d') = (S..>=) d d'

  (.>) (SyI i) (SyI i') = (S..>) i i'
  (.>) (SyD d) (SyI i)  = (S..>) d (S.sFromIntegral i)
  (.>) (SyI i) (SyD d)  = (S..>) (S.sFromIntegral i) d
  (.>) (SyD d) (SyD d') = (S..>) d d'

  (.==) (SyI i) (SyI i') = (S..==) i i'
  (.==) (SyD d) (SyI i') = (S..==) d (S.sFromIntegral i')
  (.==) (SyI i) (SyD d)  = (S..==) (S.sFromIntegral i) d
  (.==) (SyD d) (SyD d') = (S..==) d d'

  (./=) (SyI i) (SyI i') = (S../=) i i'
  (./=) (SyD d) (SyI i') = (S../=) d (S.sFromIntegral i')
  (./=) (SyI i) (SyD d)  = (S../=) (S.sFromIntegral i) d
  (./=) (SyD d) (SyD d') = (S../=) d d'

instance Prim S.SBool S.SInt8 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt16 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt32 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt64 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SDouble where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInteger where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

-- | make prop mergeable so choices can use symbolic conditionals
instance S.Mergeable (Prop' a) where
  symbolicMerge _ b thn els
    | Just result <- S.unliteral b = if result then thn else els
  symbolicMerge _ _ _ _ = undefined -- quite -WALL

-- | We can treat a variational proposition as a boolean formulae
class Boolean b where
  true  :: b
  false :: b
  bnot  :: b -> b
  (&&&) :: b -> b -> b
  (|||) :: b -> b -> b

  (<=>) :: b -> b -> b
  a <=> b = (a ==> b) &&& (b ==> a)

  (==>) :: b -> b -> b
  a ==> b = bnot a ||| b

  (<+>) :: b -> b -> b
  a <+> b = (a ||| b) &&& bnot (a &&& b)

instance Boolean S.SBool where
  true  = S.sTrue
  false = S.sFalse
  bnot  = S.sNot
  (&&&) = (S..&&)
  (|||) = (S..||)
  (<=>) = (S..<=>)
  {-# INLINE true #-}
  {-# INLINE false #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (|||) #-}
  {-# INLINE (<=>) #-}

instance Boolean Bool where
  true  =  True
  false = False
  bnot  = not
  (&&&) = (&&)
  (|||) = (||)
  {-# INLINE true  #-}
  {-# INLINE false #-}
  {-# INLINE bnot  #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (|||) #-}


instance Boolean b => Boolean (S.Symbolic b) where
  true  = return true
  false = return false
  bnot  = fmap bnot
  (&&&) = liftM2 (&&&)
  (|||) = liftM2 (|||)
  {-# INLINE true  #-}
  {-# INLINE false #-}
  {-# INLINE bnot  #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (|||) #-}

instance Boolean (Prop' a) where
  true  = LitB True
  false = LitB False
  bnot  = OpB Not
  (&&&) = OpBB And
  (|||) = OpBB Or
  (<+>) = OpBB XOr
  (==>) = OpBB Impl
  (<=>) = OpBB Eqv

instance Boolean VariantContext where
  true  = VariantContext $ LitB True
  false = VariantContext $ LitB False
  bnot  = VariantContext . OpB Not . getVarFormula
  (&&&) (getVarFormula -> x) (getVarFormula -> y) = VariantContext $ OpBB And    x y
  (|||) (getVarFormula -> x) (getVarFormula -> y) = VariantContext $ OpBB Or     x y
  (<+>) (getVarFormula -> x) (getVarFormula -> y) = VariantContext $ OpBB XOr    x y
  (==>) (getVarFormula -> x) (getVarFormula -> y) = VariantContext $ OpBB Impl   x y
  (<=>) (getVarFormula -> x) (getVarFormula -> y) = VariantContext $ OpBB Eqv    x y

-- | Boilerplate to make Num (NExpr' a) work out
instance Num NPrim where
  fromInteger = I . fromInteger
  abs = abs
  negate = negate
  signum = signum
  (+) = (+)
  (-) = (-)
  (*) = (*)

-- | We can treat Variational integer expressions like nums
instance Num (NExpr' a) where
  fromInteger = LitI . fromInteger
  abs    = OpI Abs
  negate = OpI Neg
  signum = OpI Sign
  (+)    = OpII Add
  (-)    = OpII Sub
  (*)    = OpII Mult

-- | the other num instances
instance PrimN (NExpr' a) where
  (./) = OpII Div
  (.%) = OpII Mod

instance Prim (Prop' d) (NExpr' d) where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB EQ
  (./=) = OpIB NEQ
  (.>=) = OpIB GTE
  (.>)  = OpIB GT


-- | conveniences
instance (NFData a) => NFData (Prop' a)
instance (NFData a) => NFData (NExpr' a)
instance NFData a => NFData (ExRefType a)
instance NFData NPrim
instance NFData B_B
instance NFData N_N
instance NFData BB_B
instance NFData NN_B
instance NFData NN_N
