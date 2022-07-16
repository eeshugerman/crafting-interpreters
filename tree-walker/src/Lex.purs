module Lex (Expr(..), readExpr, Literal(..), BinaryOp(..), UnaryOp(..)) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy as Lazy
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Show.Generic (genericShow)
import Parsing as P
import Parsing.Combinators (try, (<?>))
import Parsing.Expr (buildExprParser)
import Parsing.Language as L
import Parsing.String (char, eof)
import Parsing.String.Basic (alphaNum, letter)
import Parsing.Token as T

data Expr
  = Literal Literal
  | UnaryExpr UnaryOp Expr
  | BinaryExpr Expr BinaryOp Expr
  | GroupingExpr Expr

data Literal
  = LoxNumber Number
  | LoxString String
  | LoxBool Boolean
  | Nil

data BinaryOp
  = Minus
  | Plus
  | Slash
  | Star
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual

data UnaryOp = Bang

derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where
  show = \x -> genericShow x

derive instance genericLiteral :: Generic Literal _
instance showLiteral :: Show Literal where
  show = genericShow

derive instance genericUnaryOp :: Generic UnaryOp _
instance showUnaryOp :: Show UnaryOp where
  show = genericShow

derive instance genericBinaryOp :: Generic BinaryOp _
instance showBinaryOp :: Show BinaryOp where
  show = genericShow

derive instance eqExpr :: Eq Expr
derive instance eqLiteral :: Eq Literal
derive instance eqUnaryOp :: Eq UnaryOp
derive instance eqBinaryOp :: Eq BinaryOp

style :: T.LanguageDef
style = T.LanguageDef (T.unGenLanguageDef L.emptyDef)
  { commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , nestedComments = true
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  , opStart =
      T.oneOf
        [ '-'
        , '+'
        , '/'
        , '*'
        , '!'
        , '='
        , '>'
        , '<'
        ]
  , opLetter = char '='
  , reservedNames =
      [ "and"
      , "class"
      , "else"
      , "false"
      , "for"
      , "fun"
      , "if"
      , "nil"
      , "or"
      , "print"
      , "return"
      , "super"
      , "this"
      , "true"
      , "var"
      , "while"
      ]
  , reservedOpNames =
      [ "-"
      , "+"
      , "/"
      , "*"
      , "!"
      , "!="
      , "="
      , "=="
      , ">"
      , ">="
      , "<"
      , "<="
      ]
  , caseSensitive = true
  }

lexer :: T.GenTokenParser String Identity
lexer = T.makeTokenParser style

parseBinaryOp :: P.Parser String BinaryOp
parseBinaryOp = try $ do
  lexer.reservedOp "-" *> pure Minus
    <|> lexer.reservedOp "+" *> pure Plus
    <|> lexer.reservedOp "/" *> pure Slash
    <|> lexer.reservedOp "*" *> pure Star
    <|> lexer.reservedOp "!=" *> pure BangEqual
    <|> lexer.reservedOp "=" *> pure Equal
    <|> lexer.reservedOp "==" *> pure EqualEqual
    <|> lexer.reservedOp ">" *> pure Greater
    <|> lexer.reservedOp ">=" *> pure GreaterEqual
    <|> lexer.reservedOp "<" *> pure Less
    <|> lexer.reservedOp "<=" *> pure LessEqual

parseUnaryOp :: P.Parser String UnaryOp
parseUnaryOp = lexer.reservedOp "!" *> pure Bang

parseBool :: P.Parser String Expr
parseBool = do
  (lexer.reserved "true" *> (pure $ Literal $ LoxBool true))
    <|> (lexer.reserved "false" *> (pure $ Literal $ LoxBool false))

parseNumber :: P.Parser String Expr
parseNumber =
  map (Literal <<< LoxNumber)
    $ try lexer.float
        <|> try (char '+' *> lexer.float)
        <|> try (char '-' *> (map negate lexer.float))
        <|> try (map toNumber lexer.integer)

parseUnaryExpr :: P.Parser String Expr -> P.Parser String Expr
parseUnaryExpr p = try $ do
  op <- parseUnaryOp
  expr <- p
  pure $ UnaryExpr op expr

parseBinaryExpr :: P.Parser String Expr -> P.Parser String Expr
parseBinaryExpr p = try $ do
  a <- p
  op <- parseBinaryOp
  b <- p
  pure $ BinaryExpr a op b

parseGroupingExpr :: P.Parser String Expr -> P.Parser String Expr
parseGroupingExpr p = do
  expr <- try $ lexer.parens p
  pure $ GroupingExpr expr

parseExpr :: P.Parser String Expr
parseExpr = Lazy.fix $ \p ->
  parseBool
    <|> parseNumber
    <|> parseGroupingExpr p
    <|> parseUnaryExpr p
    <|> parseBinaryExpr p

-- parseExpr = buildExprParser table term <?> "expression"
-- -- https://hackage.haskell.org/package/parsec-3.1.15.1/docs/Text-Parsec-Expr.html
-- term = parseGroupingExpr <|>

readExpr :: String -> Either.Either P.ParseError Expr
readExpr input = P.runParser input parseExpr
