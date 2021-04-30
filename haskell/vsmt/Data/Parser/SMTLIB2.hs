-----------------------------------------------------------------------------
-- |
-- Module    : Parser.SMTLIB2
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Parser for the core variational propositional logic language should be as
-- identical as possible to SMTLIB2 except should not expose push/pop
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Parser.SMTLIB2 where

import           Control.Applicative (Alternative)
import           Control.Monad.Combinators.Expr

import qualified Control.Monad.State.Strict     as S

import           Data.Functor                   (void)
import qualified Data.Text                      as T
import           Data.Text.IO                   (readFile)
import           Data.Void                      (Void)
import           Prelude                        hiding (EQ, GT, LT, readFile)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L


import           Core.Types

-- | Meta data tracked during parsing
data MetaData = MetaData { variableCnt  :: Int -- ^ the number of variables
                         , choiceCnt    :: Int -- ^ the number of choices
                         , dimensionCnt :: Int -- ^ the number of unique dimensions
                         }

-- | the parser monad
newtype Parser a = Parser { runParser :: S.StateT MetaData (Parsec Void T.Text) a }
  deriving newtype (Functor,Applicative,Alternative,Monad  -- base
                    , S.MonadPlus, MonadParsec Void T.Text -- required for MonadParsec
                    , S.MonadState MetaData                -- and the state monad of course
                    )

-- parseFromFile ::
--      FilePath
--   -> IO (Either (ParseErrorBundle T.Text Void) (VariantContext, CheckableResult))
-- parseFromFile f = parse parser f <$> readFile f

-- parser :: Parser (VariantContext, CheckableResult)
-- parser = between sc eof go
