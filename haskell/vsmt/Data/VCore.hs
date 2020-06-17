-----------------------------------------------------------------------------
-- |
-- Module    : Data.VCore
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module that describes variational cores as an intermediate language. You can
-- think of a variational core as a cached version of a partially compiled SMT
-- program
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.SBV         as S
import qualified Data.SBV.Control as C
import qualified Control.Monad.State.Strict as St
import qualified Data.HashMap.Strict     as H

import           Data.Core.Types

-- | A variational core is a partial evaluated SMT program. From the point of
-- view of a compiler it is an intermediate result in an intermediate language.
-- Because VSMT by definition is solving many SMT problems we perform all
-- optimizations before interfacing with the solver. Hence, a variational core
-- must mirror the SMT datatypes in @Data.Core.Types@ but should be
-- monotonically less complex (i.e., less constructors)
type VariationalCore = Prog (VProp Var)

-- | Symbolic arithmetic expressions reduced to their variational essence
data VExpr a = SE SNum                         -- ^ A Symbolically accumulated Subtree
             | SII NN_N !(VExpr a) !(VExpr a)  -- ^ We main any binary connective
                                               -- in the case a branch has a choice
             | SChcI Dim (Expr a) (Expr a)     -- ^ Choices, again left lazy
             deriving (Eq, Show)               -- Show only for debugging

-- | Symbolic boolean expressions reduced to their variational essence
data VProp a = SV S.SBool                      -- ^ A Symbolically accumulated SubTrees
             | SBB BB_B !(VProp a) !(VProp a)  -- ^ Maintain Binary connectives
             | SIB NN_B !(VExpr a) !(VExpr a)  -- ^ And connectives for Expr's
             | SChcB Dim (Prop a) (Prop a)     -- ^ Choices
             deriving (Eq, Show)

type Cache = H.HashMap Var
type VariableCache = (Cache S.SBool, Cache SNum)

-- toVariationalCore :: SMTProg -> St.State VariableCache VariationalCore
-- toVariationalCore =
