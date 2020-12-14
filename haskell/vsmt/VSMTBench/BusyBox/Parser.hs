-----------------------------------------------------------------------------
-- |
-- Module    : VSMTBench.BusyBox.parser
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}

module Parser where

import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr
import           Prelude hiding (LT, EQ, GT)
import           Control.Monad (void)
import           Data.Functor ((<$))

import           Utils.VSMTBenchFramework

type Parser = Parsec Void T.Text

langParser :: Parser (Prop' T.Text)
langParser = between sc eof bExpr

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

underscore :: Parser ()
underscore = void $! symbol "_"

def :: Parser ()
def = reserved "def"

reserved :: T.Text -> Parser ()
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

bExpr :: Parser (Prop' T.Text)
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser (Prop' T.Text)
bTerm = parens bExpr
        <|> boolRef_

-- TODO probably going to be a performance hit
boolRef_ :: Parser (Prop' T.Text)
boolRef_ = do def; parens boolRef

boolRef :: Parser (Prop' T.Text)
boolRef = RefB . mconcat <$> sepBy (T.pack <$> many alphaNumChar) underscore

bOperators :: [[Operator Parser (Prop' a)]]
bOperators =
  [ [ Prefix (OpB Not <$ symbol "!") ]
  ,
    [ InfixL (OpBB And    <$ symbol "&")
    , InfixL (OpBB Or     <$ symbol "|")
    ]
  ]
