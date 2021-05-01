{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Debug
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr
import           Control.Monad (void)

import           Lang

type Parser = Parsec Void T.Text

langParser :: Parser AutoLang
langParser = between sc eof bExpr

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

underscore :: Parser ()
underscore = symbol "_" >> return ()

comma :: Parser T.Text
comma = symbol ","

dash :: Parser T.Text
dash = symbol "-"

reserved :: T.Text -> Parser ()
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

bExpr :: Parser AutoLang
bExpr = makeExprParser bTerm bOperators

bTerm :: Parser AutoLang
bTerm = choice
  [ (parens bExpr)
  , (try contextRef)
  , boolRef
  , rExpr
  , atMost1Expr
  , bad
  , (AutoLit True  <$ reserved "true")
  , (AutoLit False <$ reserved "false")
  ]

aTerm :: Parser ALang
aTerm = (parens aExpr)
        <|> (try aContextRef)
        <|> (try arithRef)
        <|> (ALit <$> integer)

aContextRef :: Parser ALang
aContextRef = do reserved "context"
                 _ <- brackets $ do
                   _ <- symbol "_"
                   reserved "evolution-context"
                 return . ACtx $ AVar "evo_ctx"

contextRef_ :: Parser (AutoLang -> AutoLang)
contextRef_ = do _ <- aContextRef
                 op <- relation
                 rhs <- integer
                 return $ Ctx op (ALit rhs)

contextRef :: Parser AutoLang
contextRef = do f <- contextRef_
                reserved "impl"
                rest <- bTerm
                return $ f rest

boolRef :: Parser AutoLang
boolRef = do reserved "feature"
             uuid <- brackets $ do
               _ <- underscore
               aVariable
             return . AutoRef $ uuid

arithRef :: Parser ALang
arithRef = do reserved "feature"
              uuid <- brackets $ do
                _ <- symbol "_"
                aVariable
              return $ AVar uuid

aVariable :: Parser T.Text
aVariable = mconcat <$> sepBy1 (T.pack <$> many alphaNumChar) dash

atMost1Expr :: Parser AutoLang
atMost1Expr = do reserved "oneonly"
                 features <- brackets (sepBy1 boolRef comma)
                 return $! atMost1 features


bOperators :: [[Operator Parser AutoLang]]
bOperators =
  [ [ Prefix (AutoNot <$ reserved "not") ]
  ,
    [ InfixL (BBinary And  <$ reserved "and")
    , InfixL (BBinary Or   <$ reserved "or")
    , InfixN (BBinary Impl <$ reserved "impl")
    , InfixN (BBinary Eqv  <$ reserved "iff")
    , InfixN (BBinary Eqv  <$ reserved "=")
    ]
  ]

aOperators :: [[Operator Parser ALang]]
aOperators =
  [ [ Prefix (Neg              <$ symbol "-")]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/")
    , InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-")
    , InfixL (ABinary Modulus  <$ symbol "%")
    ]
  ]

relation :: Parser RBOp
relation = pure EQL      <* symbol "="
           <|> pure LST  <* symbol "<"
           <|> pure GRTE <* symbol "<="
           <|> pure LSTE <* symbol ">="
           <|> pure GRT  <* symbol ">"
           <|> pure NEQL <* symbol "!="


rExpr :: Parser AutoLang
rExpr = try $ do
  a <- aTerm
  op <- relation
  b <- aTerm
  return (RBinary op a b)

bad :: Parser AutoLang
bad = do
  let parseFeature = do
        feature <- arithRef
        op      <- relation
        value   <- integer
        return (feature,op,value)
  (f,o,v) <- parens parseFeature
  let featConstraint = RBinary o f (ALit v)
  void $ symbol "="
  featSums <- aTerm
  let oneOfConstraint = RBinary EQL f featSums
  return (BBinary And featConstraint oneOfConstraint)


aExpr :: Parser ALang
aExpr = makeExprParser aTerm aOperators

-- | These are type errors!!!!!! (=) is overloaded, In SMTLIB `feature = 1`
-- produces a goal, i.e., returns a boolean but the (=) is trying to match `bool
-- = int`?. If (=) on the left is trying to return an integer then we would have
-- arithmetic assignment which is not in the language and furthermore assignment
-- will return () not an integer???
-- I counted roughly 30% of the props are like this:
-- 1514 -- failed with this parser
-- 3107 -- pass with this parser

-- meaning: summation on the right has to be true if the feature on the left is selected and otherwise has to be false
-- probably an exactly1!, with parent on the left
one :: T.Text
one = "(feature[_0e8e9baa-56f5-48d3-93dd-1f4db1d546d4] = 1) and (feature[_0e8e9baa-56f5-48d3-93dd-1f4db1d546d4]= (feature[_d02f4ce4-a772-4095-a4eb-4f7e6f63b99f] + feature[_8e61b75f-6ea5-4789-b20a-e0b8ebc9a8b4] + feature[_64f9dd7e-10bb-4e2d-9068-72b5837eb3ab] + feature[_4611cbb8-b0a7-40a0-a89b-0112b8770e01]))"

one' :: T.Text
one' = "(feature[_0e8e9baa-56f5-48d3-93dd-1f4db1d546d4] = 1)"

two :: T.Text
two = "(feature[_53e5b7e7-7ae7-44cd-a740-8d993d7eb86a] = 1) = (feature[_158db4b6-1e64-4e90-91ea-f8c7942e501f] + feature[_2f760b29-63d0-4f79-bb3e-f748fcf20382] + feature[_252935a5-1693-4c2d-97b9-5b796f1d5348])"

fileChunk :: [T.Text]
fileChunk = [ "feature[_b960bcd1-ffd8-474f-9232-646cca213681]"
        , "(context[_evolution-context] < 0) impl (feature[_b960bcd1-ffd8-474f-9232-646cca213681] impl (feature[_d1492ad4-8b92-490c-a89b-df8145237ee8] and feature[_d44790f0-b510-4940-a24e-ac0c652bd66f] and feature[_cf824b02-367f-42dd-bc2e-7e75ffc208d9]))"
        , "(context[_evolution-context] < 0) impl ((feature[_d1492ad4-8b92-490c-a89b-df8145237ee8] or feature[_d44790f0-b510-4940-a24e-ac0c652bd66f] or feature[_cf824b02-367f-42dd-bc2e-7e75ffc208d9]) impl feature[_b960bcd1-ffd8-474f-9232-646cca213681])"
        , "(context[_evolution-context] >= 0 and context[_evolution-context] < 4) impl (feature[_b960bcd1-ffd8-474f-9232-646cca213681] impl (feature[_d1492ad4-8b92-490c-a89b-df8145237ee8] and feature[_d44790f0-b510-4940-a24e-ac0c652bd66f] and feature[_cf824b02-367f-42dd-bc2e-7e75ffc208d9] and feature[_bc862939-7f8f-4f4f-9934-7512a48c4724]))"
        , "(context[_evolution-context] >= 0 and context[_evolution-context] < 4) impl ((feature[_d1492ad4-8b92-490c-a89b-df8145237ee8] or feature[_d44790f0-b510-4940-a24e-ac0c652bd66f] or feature[_cf824b02-367f-42dd-bc2e-7e75ffc208d9] or feature[_bc862939-7f8f-4f4f-9934-7512a48c4724]) impl feature[_b960bcd1-ffd8-474f-9232-646cca213681])"
        , "(context[_evolution-context] >= 4 and context[_evolution-context] < 6) impl (feature[_b960bcd1-ffd8-474f-9232-646cca213681] impl (feature[_d1492ad4-8b92-490c-a89b-df8145237ee8] and feature[_d44790f0-b510-4940-a24e-ac0c652bd66f] and feature[_cf824b02-367f-42dd-bc2e-7e75ffc208d9] and feature[_bc862939-7f8f-4f4f-9934-7512a48c4724] and feature[_697ed0f6-bf87-4557-88d6-be7d89b8a70a] and feature[_e9abfa15-d9ed-412a-9c5e-bcf8152de26b]))"
        , "(context[_evolution-context] >= 4 and context[_evolution-context] < 6) impl ((feature[_d1492ad4-8b92-490c-a89b-df8145237ee8] or feature[_d44790f0-b510-4940-a24e-ac0c652bd66f] or feature[_cf824b02-367f-42dd-bc2e-7e75ffc208d9] or feature[_bc862939-7f8f-4f4f-9934-7512a48c4724] or feature[_697ed0f6-bf87-4557-88d6-be7d89b8a70a] or feature[_e9abfa15-d9ed-412a-9c5e-bcf8152de26b]) impl feature[_b960bcd1-ffd8-474f-9232-646cca213681])"
        ]
