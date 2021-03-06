-----------------------------------------------------------------------------
-- |
-- Module    : Parser.Result
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Parser for Data.Core.Result
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser.Result where


import           Control.Monad.Combinators.Expr

import qualified Data.HashMap.Strict            as M

import           Data.Functor                   (void)
import qualified Data.Text                      as T
import           Data.Text.IO                   (readFile)
import           Data.Void                      (Void)
import           Prelude                        hiding (EQ, GT, LT, readFile)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L


import           Core.Types

type Parser = Parsec Void T.Text

parseFromFile ::
     FilePath
  -> IO (Either (ParseErrorBundle T.Text Void) (VariantContext, CheckableResult))
parseFromFile f = parse resultParser f <$> readFile f

resultParser :: Parser (VariantContext, CheckableResult)
resultParser = between sc eof go


go :: Parser (VariantContext, CheckableResult)
go = do modelHeader
        ms <- many modelRow
        s <- lexeme satFormula
        return (s, mconcat ms)

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

emptyLine :: Parser ()
emptyLine = C.newline >> C.space >> C.newline >> return ()

modelHeader :: Parser ()
modelHeader = void $ between headerL headerR (literal "Model")

satHeader :: Parser ()
satHeader = void $ between headerL headerR (literal "Sat_Model")

headerR :: Parser ()
headerR = lexeme $ void $ symbol ":="

headerL :: Parser ()
headerL = lexeme $ void $ symbol "=:"

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

literal :: T.Text -> Parser T.Text
literal = lexeme . C.string

is :: Parser ()
is = void $ symbol "-->"

parens :: Parser a -> Parser a
parens = lexeme . between (symbol "(") (symbol ")")

ite :: Parser ()
ite = void $ literal "ite"

undefined' :: Parser [(Maybe VariantContext, Value)]
undefined' = C.string "Undefined" >> return []

value :: Parser Value
value = lexeme $
  do v <- numericValue <|> boolValue
     void $ symbol "::"
     void typeTerm -- we parse the type term and throw it away, this might be
                   -- useful in the future
     return v

variable :: Parser T.Text
variable = T.pack <$> lexeme ((:) <$> C.letterChar <*> many C.alphaNumChar <?> "variable") -- C.space

typeTerm :: Parser Type
typeTerm = choice [b,i,d]
  where b = literal "Bool"    >> return TBool
        i = literal "Integer" >> return TInt
        d = literal "Double"  >> return TDouble

numericValue :: Parser Value
numericValue = choice [i,d,si,sd]
  where i = N . I <$> integer
        d = N . D <$> float
        si = N . I <$> L.signed sc integer
        sd = N . D <$> L.signed sc float

boolValue :: Parser Value
boolValue = choice [t, f]
  where t = do void $ literal "True";  return $ B True
        f = do void $ literal "False"; return $ B False

iteTerm :: Parser [(Maybe VariantContext, Value)]
iteTerm = do ite
             ctx <- lexeme (parens context <|> context)
             v   <- lexeme (parens value)
             rest <- parens iteTerm <|> iteTerm <|> undefined'
             return $ (Just ctx, v) : rest

plainValue :: Parser [(Maybe VariantContext, Value)]
plainValue = pure . (Nothing,) <$> value

modelRow :: Parser CheckableResult
modelRow = do var <- variable
              is
              val <- parens iteTerm <|> plainValue
              return . CheckableResult $ M.singleton var val

satFormula :: Parser VariantContext
satFormula = satHeader >> variantContext

dimension :: Parser Dim
dimension = lexeme $ Dim . T.pack <$> getString
  where
    getString :: Parser String
    getString = (:) <$> C.letterChar <*> many C.alphaNumChar <?> "dimension"

context :: Parser VariantContext
context = VariantContext <$> bExpr

variantContext :: Parser VariantContext
variantContext = context

bExpr :: Parser (Prop' Dim)
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser (Prop' Dim)
bTerm = choice [ RefB <$> dimension
               , parens bExpr
               , bExpr
               ] -- may need to add literals here


-- TODO probably going to be a performance hit
bOperators :: [[Operator Parser (Prop' Dim)]]
bOperators =
  [ [ Prefix $ OpB Not  <$ symbol "~" ]
  ,
    [ InfixL $ OpBB And <$ symbol "and"
    , InfixL $ OpBB Or  <$ symbol "or"
    ]
  ]
