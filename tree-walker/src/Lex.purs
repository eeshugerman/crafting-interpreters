module Lex (readExpr) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.String.Regex.Flags (dotAll)
import Parsing as P
import Parsing.Language as L
import Parsing.String (char)
import Parsing.String.Basic (alphaNum, letter)
import Parsing.Token as T

data Expr
  = Literal Literal
  | UnaryExpr Op Expr
  | BinaryExpr Expr Op Expr
  | GroupingExpr Expr

data Literal
  = LoxNumber Number
  | LoxString String
  | LoxBool Boolean
  | Nil

data Op
  = Minus
  | Plus
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual

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

parseBool :: P.ParserT String Expr
parseBool = do
  (lexer.reserved "true" *> pure $ LoxBool true) <|>
    (lexer.reserved "false" *> pure $ LoxBool false)

parseNumber :: P.Parser String Expr
parseNumber = do
  val <- lexer.naturalOrFloat
  pure $ Literal $ LoxNumber $ case val of
    Left val' -> toNumber val'
    Right val' -> val'

parseOp :: P.Parser String Op
parseOp = do
  lexer.reservedOp "-" *> pure Minus
    <|> lexer.reservedOp "+" *> pure Plus
    <|> lexer.reservedOp "/" *> pure Slash
    <|> lexer.reservedOp "*" *> pure Star
    <|> lexer.reservedOp "!" *> pure Bang
    <|> lexer.reservedOp "!=" *> pure BangEqual
    <|> lexer.reservedOp "=" *> pure Equal
    <|> lexer.reservedOp "==" *> pure EqualEqual
    <|> lexer.reservedOp ">" *> pure Greater
    <|> lexer.reservedOp ">=" *> pure GreaterEqual
    <|> lexer.reservedOp "<" *> pure Less
    <|> lexer.reservedOp "<=" *> pure LessEqual

parseUnaryExpr :: P.ParserT String Expr
parseUnaryExpr = do
  op <- parseOp
  expr <- parseExpr
  pure $ UnaryExpr op expr

-- parseBinaryExpr :: P.ParserT String Expr
parseBinaryExpr = do
  a <- parseExpr
  op <- parseOp
  b <- parseExpr
  pure $ BinaryExpr a op b

parseGroupingExpr :: P.ParserT String Expr
parseGroupingExpr = do
  expr <- lexer.parens
  pure $ GroupingExpr expr

-- parseExpr :: P.ParserT String Expr
parseExpr = parseBool <|> parseNumber <|> parseUnaryExpr <|> parseBinaryExpr <|> parseGroupingExpr

readExpr :: String -> Either P.ParseError Expr
readExpr s = P.runParser s parseExpr
