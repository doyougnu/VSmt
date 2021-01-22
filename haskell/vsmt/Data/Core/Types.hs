-----------------------------------------------------------------------------
-- |
-- Module    : Core.Types
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Internal Types for the vsmt library
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}

module Core.Types where

import           Control.Monad       (liftM2)
import           Control.DeepSeq     (NFData)
import           Data.Fixed          (mod')
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.Sequence       as Seq
import qualified Data.SBV.Trans      as S
import           Data.String         (IsString)
import           Data.Text           (Text,pack,unpack)
import           GHC.Generics        (Generic)
import           Prelude             hiding (EQ, GT, LT, lookup)


-- | A feature is a named, boolean configuration option.
type Var = Text
newtype Dim = Dim { getDim :: Text}
  deriving newtype (Eq,Ord,NFData,Hashable,IsString)

type Config  = Dim -> Bool

--unfortunately we need the maybe, see the Configurable class in Core
type PartialConfig = Dim -> Maybe Bool

-- | empty type to represent when an 'a' is total
newtype Total a = Total { unTotal :: a }

-- | an type to represent that an 'a' is plain
newtype Plain a = Plain { unPlain :: a }
  deriving (Eq, Show)

--
-- * Syntax
--

-- | A Variant formula is a possibly smt propositional formula with references
-- restricted to dimensions only. We could choose to force that the dimensions
-- \in prop they describe but this artificially restricts the expressiveness of
-- the system and is best left to the end-user
newtype VariantContext = VariantContext { getVarFormula :: Prop' Dim }
                       deriving stock (Eq,Generic,Ord,Show)
                       deriving newtype NFData

toVariantContext :: Prop' Dim -> VariantContext
toVariantContext = VariantContext

-- | An SMT Program is a sequence of statements interacting with the base solver
type Prog = Seq.Seq
type Proposition = Prop' Var
type NExpression = NExpr' Var
type SMTProg = Prog (Stmt Proposition)

instance Semigroup VariantContext where
  (<>) (getVarFormula -> x) (getVarFormula -> y) = VariantContext (OpBB Or x y)

instance Monoid VariantContext where
  mempty = true

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
              deriving (Eq,Ord,Show,Generic)

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
  deriving stock (Eq,Generic,Show,Functor,Traversable,Foldable,Ord)

deriving instance Hashable a => Hashable (Prop' a)

-- | Numerical Expressions with Choices
data NExpr' a
  = LitI NPrim                        -- ^ Arithmetic Literals
  | RefI !(ExRefType a)               -- ^ Arithmetic References
  | OpI  N_N  !(NExpr' a)             -- ^ Unary Operators
  | OpII NN_N !(NExpr' a) !(NExpr' a) -- ^ Binary Operators
  | ChcI !Dim  (NExpr' a) (NExpr' a)  -- ^ SMT Choices
  deriving stock (Eq,Generic,Show,Functor,Traversable,Foldable,Ord)

deriving instance Hashable a => Hashable (NExpr' a)

-- | Types of references
data Type = TBool
          | TInt
          | TDouble
          deriving (Eq,Generic,Show,Ord)


data Value = N NPrim | B Bool
  deriving stock (Eq,Show,Ord,Generic)
  deriving anyclass Hashable


newtype CheckableResult =
  CheckableResult { getChkResult :: M.HashMap Var [(Maybe VariantContext, Value)] }
  deriving newtype (Eq,Show,Semigroup,Monoid)

toCheckableResult :: [(Var, [(Maybe VariantContext, Value)])] -> CheckableResult
toCheckableResult = CheckableResult . M.fromList


data ExRefType a = ExRefTypeI a | ExRefTypeD a
  deriving stock (Eq,Generic,Show,Ord,Functor,Traversable,Foldable)

deriving instance Hashable a => Hashable (ExRefType a)

-- | data constructor for Numeric operations
data NPrim = I !Integer | D {-# UNPACK #-} !Double
  deriving stock (Eq,Generic,Ord,Show)
  deriving anyclass Hashable

-- | Unary Numeric Operator
data N_N = Neg | Abs | Sign
  deriving stock (Eq,Generic,Ord,Show)
  deriving anyclass Hashable

-- | Binary Boolean operators
data B_B = Not
  deriving stock (Eq,Generic,Ord,Show)
  deriving anyclass Hashable

-- | Binary Numeric Operators
data NN_N = Add | Sub | Mult | Div | Mod
  deriving stock (Eq,Generic,Ord,Show)
  deriving anyclass Hashable

-- | Binary Boolean operators
data BB_B = And | Or | Impl | Eqv | XOr
  deriving stock (Eq,Generic,Ord,Show)
  deriving anyclass Hashable

-- | Binary Numeric predicate operators
data NN_B = LT | LTE | GT | GTE | EQ | NEQ
  deriving stock (Eq,Generic,Ord,Show)
  deriving anyclass Hashable

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

-- TODO #2
iLit :: Integer -> NExpr' a
iLit = LitI . I
{-# INLINE iLit #-}

dLit :: Double -> NExpr' a
dLit = LitI . D
{-# INLINE dLit #-}

bChc :: Text -> Prop' a -> Prop' a -> Prop' a
bChc = ChcB . Dim
{-# INLINE bChc #-}

iChc :: Text -> NExpr' a -> NExpr' a -> NExpr' a
iChc = ChcI . Dim
{-# INLINE iChc #-}

toDim :: String -> Dim
toDim = Dim . pack
instance Show Dim where show = unpack . getDim

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

instance Num Value where
  fromInteger = N . I

  abs (N (I i)) = N . I $ abs i
  abs (N (D i)) = N . D $ abs i
  abs (B  _) = error "Num called on bool"

  negate (N (I i)) = N . I $ negate i
  negate (N (D i)) = N . D $ negate i
  negate (B _)     = error "Num called on bool"

  signum (N (I i)) = N . I $ signum i
  signum (N (D i)) = N . D $ signum i
  signum (B _)     = error "Num called on bool"

  (N (I i)) + (N (I j)) = N . I $ i + j
  (N (D i)) + (N (I j)) = N . D $ i + fromInteger j
  (N (I i)) + (N (D j)) = N . D $ fromInteger i + j
  (N (D i)) + (N (D j)) = N . D $ i + j
  _         + _         = error "Addition called on boolean"

  (N (I i)) * (N (I j)) = N . I $ i * j
  (N (D i)) * (N (I j)) = N . D $ i * fromInteger j
  (N (I i)) * (N (D j)) = N . D $ fromInteger i * j
  (N (D i)) * (N (D j)) = N . D $ i * j
  _         * _         = error "Multiplication called on boolean"

instance Fractional Value where
  (N (I i)) / (N (I j)) = N . D $ fromInteger i / fromInteger j
  (N (D i)) / (N (I j)) = N . D $ i / fromInteger j
  (N (I i)) / (N (D j)) = N . D $ fromInteger i / j
  (N (D i)) / (N (D j)) = N . D $ i / j
  _         / _         = error "Division called on boolean"

  fromRational = N . D . fromRational

instance PrimN Value where
  (N (I i)) ./ (N (I j)) = N . I $ i ./ j
  (N (D i)) ./ (N (I j)) = N . D $ i ./ fromInteger j
  (N (I i)) ./ (N (D j)) = N . D $ fromInteger i ./ j
  (N (D i)) ./ (N (D j)) = N . D $ i ./ j
  _         ./ _         = error "Division called on boolean"

  (N (I i)) .% (N (I j)) = N . I $ i .% j
  (N (D i)) .% (N (I j)) = N . D $ i .% fromInteger j
  (N (I i)) .% (N (D j)) = N . D $ fromInteger i .% j
  (N (D i)) .% (N (D j)) = N . D $ i .% j
  _         .% _         = error "Modulus called on boolean"

instance Integral Value where
  toInteger (N (I i)) = i
  toInteger (N (D _)) = undefined
  toInteger (B _)     = error "ToInteger called on Boolean"

  quotRem (N (I i)) (N (I j)) = (N (I a), N (I b))
    where (a, b) = quotRem i j
  quotRem _         _         = error "QuotRem called on non-integral"

instance Real Value where
  toRational (N (I i)) = toRational i
  toRational (N (D d)) = toRational d
  toRational _         = error "ToRational called on Boolean"

instance Enum Value where
  toEnum i = N (I (toInteger i))
  fromEnum (N (I i)) = fromIntegral i
  fromEnum (N (D _)) = error "Enum on double"
  fromEnum (B b)     = fromEnum b

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

instance Fractional (NExpr' a) where
  (/) = (./)

  fromRational = LitI . D . fromRational

-- | conveniences
instance (NFData a) => NFData (Prop' a)
instance (NFData a) => NFData (NExpr' a)
instance NFData a => NFData (ExRefType a)
instance NFData Value
instance NFData NPrim
instance NFData B_B
instance NFData N_N
instance NFData BB_B
instance NFData NN_B
instance NFData NN_N
