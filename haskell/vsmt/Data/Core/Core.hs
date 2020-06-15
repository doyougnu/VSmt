{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}


data Type :: * -> * where
  TInt    :: Type Int
  TBool   :: Type Bool
  TFloat  :: Type Float
  TArrow  :: Type a -> Type b -> Type (a -> b)

data Bop :: * -> * -> * where
  Add  :: Bop Int Int
  Sub  :: Bop Int Int
  Eq   :: Bop Int Bool
  Lt   :: Bop Int Bool
  And  :: Bop Bool Bool
  Or   :: Bop Bool Bool

data ArithOp = Addd

class (Numeric a, Numeric b, Numeric c) => NumericOp a b c | a b -> c where
  isNumeric :: a -> b -> Type c

class Num a => Numeric a where numeric :: a -> Type a
instance Numeric Float where numeric _ = TFloat
instance Numeric Int   where numeric _ = TInt

instance NumericOp Int   Int   Int   where isNumeric _ _ = TInt
instance NumericOp Int   Float Float where isNumeric _ _ = TFloat
instance NumericOp Float Int   Float where isNumeric _ _ = TFloat
instance NumericOp Float Float Float where isNumeric _ _ = TFloat


class TypeOf a where typeOf :: a -> Type a

instance TypeOf Int where typeOf _ = TInt
instance TypeOf Bool where typeOf _ = TBool
instance TypeOf Float where typeOf _ = TFloat

class TypeOfExpr a where
  typeOfExpr :: Expr a -> Type a

instance TypeOfExpr Int where typeOfExpr _ = TInt
instance TypeOfExpr Float where typeOfExpr _ = TFloat
instance TypeOfExpr Bool where typeOfExpr _ = TBool

data Expr :: * -> * where
  Lit     :: TypeOf a => a -> Expr a
  Var     :: String -> Type a -> Expr a
  Lambda  :: String -> Type a -> Expr b -> Expr (a -> b)
  App     :: Expr (a -> b) -> Expr a -> Expr b
  Bop     :: Bop a b -> Expr a -> Expr a -> Expr b
  Arith   :: (NumericOp a b c) => Expr a -> ArithOp -> Expr b -> Expr c
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a
  Lift    :: a -> Type a -> Expr a

-- >>> :t run $ Arith (Lit 3) Addd (Lit 3)
-- run $ Arith (Lit 3) Addd (Lit 3)
--   :: (NumericOp a1 b a2, TypeOf a1, TypeOf b) => a2

-- >>> :t run $ Arith (Lit 2.2 ) Addd (Lit 3 )
-- Arith (Lit 2.2 ) Addd (Lit 3 )
--   :: (NumericOp a b c, Fractional a, TypeOf a, TypeOf b) => Expr c

run :: Expr a -> a
run = undefined
