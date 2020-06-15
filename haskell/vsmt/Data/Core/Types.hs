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


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Core.Types where

import           Control.Monad         (liftM2)
import           Control.DeepSeq       (NFData)
import           Data.Data             (Data, Typeable)
import           Data.Text             (pack, Text)
import           Data.Fixed            (mod')
import qualified Data.SBV              as S
import           Data.String           (IsString)
import           GHC.Generics          (Generic)
import           Prelude               hiding (EQ, GT, LT, lookup)


-- | A feature is a named, boolean configuration option.
type Var = Text
type Dim = Text
type Config  = Dim -> Bool

--
-- * Syntax
--

-- | A Variant formula is a possibly smt propositional formula with references
-- restricted to dimensions only. We could choose to force that the dimensions
-- \in prop they describe but this artificially restricts the expressiveness of
-- the system and is best left to the end-user
newtype VariantFormula = VariantFormula { getVarFormula :: (Prop Dim) }
  deriving (Eq,Generic,Ord,Show)

instance Semigroup VariantFormula where
  (<>) (getVarFormula -> x) (getVarFormula -> y) = VariantFormula (OpBB Or x y)

instance Monoid VariantFormula where mempty = false
                                     mappend = (<>)

-- | Top level Syntax
data Prog a = Assert !(Prop a)  -- ^ constraint the prop
            | Call SolverOp     -- ^ side effectual interact with the solver
            | IfThenElse !(Prop a) (Prop a) (Prop a)
            deriving (Eq,Generic,Ord,Functor,Traversable,Foldable)

data SolverOp = IsSat      -- ^ call sat
              | SatModel   -- ^ sat with model
              | Optimize   -- ^ optimize
              | Prove      -- ^ Prove, this could just be sat $ negate prop though
              | CodeGen    -- ^ generate code
              deriving (Eq,Ord,Show)

-- | Boolean expressions with choices, value and spine strict
-- TODO combine using GADTS
data Prop a
   = LitB Bool                       -- ^ boolean literals
   | RefB !a                         -- ^ Bool References
   | OpB  B_B  !(Prop a)             -- ^ Unary Operators
   | OpBB BB_B !(Prop a)  !(Prop a)  -- ^ Binary Operators
   | OpIB NN_B !(Expr a) !(Expr a)   -- ^ SMT Arithmetic
   -- we leave choices to be lazy for performance in selection/configuration. It
   -- may be the case that one alternative is never selected for
   | ChcB !Dim (Prop a) (Prop a)     -- ^ Choices
  deriving (Eq,Generic,Show,Typeable,Functor,Traversable,Foldable,Ord)

-- | Expressions with Choices
data Expr a
  = LitI NPrim                      -- ^ Arithmetic Literals
  | Ref  RefN !a                    -- ^ Arithmetic References
  | OpI  N_N  !(Expr a)             -- ^ Unary Operators
  | OpII NN_N !(Expr a) !(Expr a)   -- ^ Binary Operators
  | ChcI Dim  (Expr a) (Expr a)     -- ^ SMT Choices
  deriving (Eq,Generic,Show,Typeable,Functor,Traversable,Foldable,Ord)

-- | Mirroring NPrim with Symbolic types for the solver
data SNum = SI !S.SInt64
          | SD !S.SDouble
          deriving (Eq, Generic,Show)

-- | data constructor for Numeric operations
data NPrim = I {-# UNPACK #-} !Int | D {-# UNPACK #-} !Double
  deriving (Eq,Generic,Typeable,Ord,Show)

-- | Reference types
data RefN = RefI | RefD deriving (Eq,Generic,Typeable,Ord,Show)

-- | Unary Numeric Operator
data N_N = Neg | Abs | Sign deriving (Eq,Generic,Data,Typeable,Ord,Show)

-- | Binary Boolean operators
data B_B = Not deriving (Eq,Generic,Data,Typeable,Ord,Show)

-- | Binary Numeric Operators
data NN_N = Add | Sub | Mult | Div | Mod deriving (Eq,Generic,Data,Typeable,Ord,Show)

-- | Binary Boolean operators
data BB_B = And | Or | Impl | BiImpl | XOr deriving (Eq,Generic,Data,Typeable,Ord,Show)

-- | Binary Numeric predicate operators
data NN_B = LT | LTE | GT | GTE | EQ | NEQ deriving (Eq,Generic,Data,Typeable,Ord,Show)

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
iRef :: IsString a => a -> Expr a
iRef = Ref RefI
{-# INLINE iRef #-}

iLit :: Int -> Expr a
iLit = LitI . I
{-# INLINE iLit #-}

dLit :: Double -> Expr a
dLit = LitI . D
{-# INLINE dLit #-}

dRef :: IsString a => a -> Expr a
dRef = Ref RefD
{-# INLINE dRef #-}

bRef :: IsString a => a -> Prop a
bRef = RefB
{-# INLINE bRef #-}

bChc :: Dim -> Prop a -> Prop a -> Prop a
bChc = ChcB
{-# INLINE bChc #-}

iChc :: Dim -> Expr a -> Expr a -> Expr a
iChc = ChcI
{-# INLINE iChc #-}

-- | Begin primitive instances

instance PrimN Int where
  (./) = div
  (.%) = mod

instance PrimN Double where
  (./) = (/)
  (.%) = mod'

instance Prim Bool Int where
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

instance Prim (Prop a) Int where
  (.<)  i j = OpIB LT  (LitI $ I i) (LitI $ I j)
  (.<=) i j = OpIB LTE (LitI $ I i) (LitI $ I j)
  (.==) i j = OpIB EQ  (LitI $ I i) (LitI $ I j)
  (./=) i j = OpIB NEQ (LitI $ I i) (LitI $ I j)
  (.>=) i j = OpIB GTE (LitI $ I i) (LitI $ I j)
  (.>)  i j = OpIB GT  (LitI $ I i) (LitI $ I j)

instance Prim (Prop a) Double where
  (.<)  i j = OpIB LT  (LitI $ D i) (LitI $ D j)
  (.<=) i j = OpIB LTE (LitI $ D i) (LitI $ D j)
  (.==) i j = OpIB EQ  (LitI $ D i) (LitI $ D j)
  (./=) i j = OpIB NEQ (LitI $ D i) (LitI $ D j)
  (.>=) i j = OpIB GTE (LitI $ D i) (LitI $ D j)
  (.>)  i j = OpIB GT  (LitI $ D i) (LitI $ D j)

-- * SBV instances

-- | we'll need to mirror the NPrim data type in SBV via SNum
instance Num SNum where
  fromInteger = SI . S.literal . fromInteger

  abs (SI i) = SI $ abs i
  abs (SD d) = SD $ S.fpAbs d

  negate (SI i) = SI $ negate i
  negate (SD d) = SD $ S.fpNeg d

  signum (SI i) = SI $ signum i
  signum (SD d) = SD $ signum (S.fromSDouble S.sRoundNearestTiesToAway d)

  (SI i) + (SI i') = SI $ i + i'
  (SD d) + (SI i)  = SD $ d + (S.sFromIntegral i)
  (SI i) + (SD d)  = SD $ (S.sFromIntegral i) + d
  (SD d) + (SD d') = SD $ d + d'

  (SI i) - (SI i') = SI $ i - i'
  (SD d) - (SI i)  = SD $ d - S.sFromIntegral i
  (SI i) - (SD d)  = SD $ S.sFromIntegral i - d
  (SD d) - (SD d') = SD $ d - d'

  (SI i) * (SI i') = SI $ i * i'
  (SD d) * (SI i)  = SD $ d * S.sFromIntegral i
  (SI i) * (SD d)  = SD $ d * S.sFromIntegral i
  (SD d) * (SD d') = SD $ d * d'

instance PrimN SNum where
  (SI i) ./ (SI i') = SI $ i ./ i'
  (SD d) ./ (SI i)  = SD $ d ./ (S.sFromIntegral i)
  (SI i) ./ (SD d)  = SD $ (S.sFromIntegral i) ./ d
  (SD d) ./ (SD d') = SD $ d ./ d'


  (SI i) .% (SI i') = SI $ i .% i'
  (SD d) .% (SI i)  = SI $ (S.fromSDouble S.sRoundNearestTiesToAway d) .% i
  (SI i) .% (SD d)  = SI $ i .% (S.fromSDouble S.sRoundNearestTiesToAway d)
  (SD d) .% (SD d') = SD $ S.fpRem d d'

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

instance S.Mergeable SNum where
  symbolicMerge _ b thn els
    | Just result <- S.unliteral b = if result then thn else els
    | otherwise = els

instance S.EqSymbolic SNum where
  (.==) (SI i) (SI i') = (S..==) i i'
  (.==) (SD d) (SI i') = (S..==) d (S.sFromIntegral i')
  (.==) (SI i) (SD d)  = (S..==) (S.sFromIntegral i) d
  (.==) (SD d) (SD d') = (S..==) d d'

  (./=) (SI i) (SI i') = (S../=) i i'
  (./=) (SD d) (SI i') = (S../=) d (S.sFromIntegral i')
  (./=) (SI i) (SD d)  = (S../=) (S.sFromIntegral i) d
  (./=) (SD d) (SD d') = (S../=) d d'

instance S.OrdSymbolic SNum where
  (.<) (SI i) (SI i') = (S..<) i i'
  (.<) (SD d) (SI i)  = (S..<) d (S.sFromIntegral i)
  (.<) (SI i) (SD d)  = (S..<) (S.sFromIntegral i) d
  (.<) (SD d) (SD d') = (S..<) d d'

  (.<=) (SI i) (SI i') = (S..<=) i i'
  (.<=) (SD d) (SI i)  = (S..<=) d (S.sFromIntegral i)
  (.<=) (SI i) (SD d)  = (S..<=) (S.sFromIntegral i) d
  (.<=) (SD d) (SD d') = (S..<=) d d'

  (.>=) (SI i) (SI i') = (S..>=) i i'
  (.>=) (SD d) (SI i)  = (S..>=) d (S.sFromIntegral i)
  (.>=) (SI i) (SD d)  = (S..>=) (S.sFromIntegral i) d
  (.>=) (SD d) (SD d') = (S..>=) d d'

  (.>) (SI i) (SI i') = (S..>) i i'
  (.>) (SD d) (SI i)  = (S..>) d (S.sFromIntegral i)
  (.>) (SI i) (SD d)  = (S..>) (S.sFromIntegral i) d
  (.>) (SD d) (SD d') = (S..>) d d'

instance Prim S.SBool SNum where
  (.<) (SI i) (SI i') = (S..<) i i'
  (.<) (SD d) (SI i)  = (S..<) d (S.sFromIntegral i)
  (.<) (SI i) (SD d)  = (S..<) (S.sFromIntegral i) d
  (.<) (SD d) (SD d') = (S..<) d d'

  (.<=) (SI i) (SI i') = (S..<=) i i'
  (.<=) (SD d) (SI i)  = (S..<=) d (S.sFromIntegral i)
  (.<=) (SI i) (SD d)  = (S..<=) (S.sFromIntegral i) d
  (.<=) (SD d) (SD d') = (S..<=) d d'

  (.>=) (SI i) (SI i') = (S..>=) i i'
  (.>=) (SD d) (SI i)  = (S..>=) d (S.sFromIntegral i)
  (.>=) (SI i) (SD d)  = (S..>=) (S.sFromIntegral i) d
  (.>=) (SD d) (SD d') = (S..>=) d d'

  (.>) (SI i) (SI i') = (S..>) i i'
  (.>) (SD d) (SI i)  = (S..>) d (S.sFromIntegral i)
  (.>) (SI i) (SD d)  = (S..>) (S.sFromIntegral i) d
  (.>) (SD d) (SD d') = (S..>) d d'

  (.==) (SI i) (SI i') = (S..==) i i'
  (.==) (SD d) (SI i') = (S..==) d (S.sFromIntegral i')
  (.==) (SI i) (SD d)  = (S..==) (S.sFromIntegral i) d
  (.==) (SD d) (SD d') = (S..==) d d'

  (./=) (SI i) (SI i') = (S../=) i i'
  (./=) (SD d) (SI i') = (S../=) d (S.sFromIntegral i')
  (./=) (SI i) (SD d)  = (S../=) (S.sFromIntegral i) d
  (./=) (SD d) (SD d') = (S../=) d d'

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
instance S.Mergeable (Prop a) where
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
  a ==> b = (bnot a) ||| b

  (<+>) :: b -> b -> b
  a <+> b = (a ||| b) &&& (bnot (a &&& b))

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

instance Boolean (Prop a) where
  true  = LitB True
  false = LitB False
  bnot  = OpB Not
  (&&&) = OpBB And
  (|||) = OpBB Or
  (<+>) = OpBB XOr
  (==>) = OpBB Impl
  (<=>) = OpBB BiImpl

instance Boolean VariantFormula where
  true  = VariantFormula $ LitB True
  false = VariantFormula $ LitB False
  bnot  = VariantFormula . OpB Not . getVarFormula
  (&&&) (getVarFormula -> x) (getVarFormula -> y) = VariantFormula $ OpBB And    x y
  (|||) (getVarFormula -> x) (getVarFormula -> y) = VariantFormula $ OpBB Or     x y
  (<+>) (getVarFormula -> x) (getVarFormula -> y) = VariantFormula $ OpBB XOr    x y
  (==>) (getVarFormula -> x) (getVarFormula -> y) = VariantFormula $ OpBB Impl   x y
  (<=>) (getVarFormula -> x) (getVarFormula -> y) = VariantFormula $ OpBB BiImpl x y

-- | Boilerplate to make Num (Expr a) work out
instance Num (NPrim) where
  fromInteger = I . fromInteger
  abs = abs
  negate = negate
  signum = signum
  (+) = (+)
  (-) = (-)
  (*) = (*)

-- | We can treat Variational integer expressions like nums
instance Num (Expr a) where
  fromInteger = LitI . fromInteger
  abs    = OpI Abs
  negate = OpI Neg
  signum = OpI Sign
  (+)    = OpII Add
  (-)    = OpII Sub
  (*)    = OpII Mult

-- | the other num instances
instance PrimN (Expr a) where
  (./) = OpII Div
  (.%) = OpII Mod

instance Prim (Prop d) (Expr d) where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB EQ
  (./=) = OpIB NEQ
  (.>=) = OpIB GTE
  (.>)  = OpIB GT


-- | conveniences
instance (NFData a) => NFData (Prop a)
instance (NFData a) => NFData (Expr a)
instance NFData NPrim
instance NFData B_B
instance NFData N_N
instance NFData BB_B
instance NFData NN_B
instance NFData NN_N
instance NFData RefN
