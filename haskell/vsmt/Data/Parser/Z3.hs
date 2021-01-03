-----------------------------------------------------------------------------
-- |
-- Module    : Parser.Z3
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Parser for Z3 model bindings
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE OverloadedStrings #-}

module Parser.Z3 where

-- import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L

import           Data.Text hiding               (empty)
import           Data.Void                      (Void)
import           Control.Monad                  (void)
import           Core.Types  (Var,Value(..))

type Parser = Parsec Void Text

parseModel :: Text -> [(Var,Value)]
parseModel s = case run of
                 Left _  -> mempty -- This should never error so throw away the either
                 Right a -> a
  where run =  parse go "" s

go :: Parser [(Var,Value)]
go = between sc eof (row `sepEndBy` C.newline)

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

exclamation :: Parser Text
exclamation = symbol "!"

arrow :: Parser Text
arrow = symbol "->"

variable :: Parser Text
variable = pack <$> lexeme ((:) <$> C.letterChar <*> manyTill C.alphaNumChar exclamation <?> "variable")

pKeyword :: Text -> Parser Text
pKeyword keyword = (C.string keyword <* notFollowedBy C.alphaNumChar)

true :: Parser Value
true = pKeyword "true" *> pure (B True)

false :: Parser Value
false = pKeyword "false" *> pure (B False)

-- TODO instances for integers and float
bool :: Parser Value
bool = true <|> false

row :: Parser (Var, Value)
row = do v <- lexeme variable
         void $ skipManyTill C.digitChar C.space1
         void arrow
         val <- bool
         return (v,val)
