module Parse (Expr(..), readExpr, Literal(..), BinaryOp(..), UnaryOp(..)) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy as Lazy
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Show.Generic (genericShow)
import Parsing as P
import Parsing.Combinators (choice, try)
import Parsing.Language as L
import Parsing.String (char)
import Parsing.String.Basic (alphaNum, letter)
import Parsing.Token as T

data Expr
  = Literal Literal
  | UnaryExpr UnaryOp Expr
  | BinaryExpr BinaryOp Expr Expr
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

data UnaryOp = Bang | Negative

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

binaryOp :: P.Parser String BinaryOp
binaryOp = choice
  [ lexer.reservedOp "-" *> pure Minus
  , lexer.reservedOp "+" *> pure Plus
  , lexer.reservedOp "/" *> pure Slash
  , lexer.reservedOp "*" *> pure Star
  , lexer.reservedOp "!=" *> pure BangEqual
  , lexer.reservedOp "=" *> pure Equal
  , lexer.reservedOp "==" *> pure EqualEqual
  , lexer.reservedOp ">" *> pure Greater
  , lexer.reservedOp ">=" *> pure GreaterEqual
  , lexer.reservedOp "<" *> pure Less
  , lexer.reservedOp "<=" *> pure LessEqual
  ]

unaryOp :: P.Parser String UnaryOp
unaryOp = choice
  [ lexer.reservedOp "!" *> pure Bang
  , lexer.reservedOp "-" *> pure Negative
  ]

boolLiteral :: P.Parser String Expr
boolLiteral = map (Literal <<< LoxBool) $ choice
  [ (lexer.reserved "true" *> pure true)
  , (lexer.reserved "false" *> pure false)
  ]

numberLiteral :: P.Parser String Expr
numberLiteral =
  map (Literal <<< LoxNumber) $ choice
    [ try lexer.float
    , map toNumber lexer.integer
    ]

nilLiteral :: P.Parser String Expr
nilLiteral = lexer.reserved "nil" *> (pure $ Literal $ Nil)

unaryExpr :: P.Parser String Expr -> P.Parser String Expr
unaryExpr p = do
  op <- unaryOp
  expr' <- p
  pure $ UnaryExpr op expr'

binaryExpr :: P.Parser String Expr -> P.Parser String Expr
binaryExpr p = try do
  a <- nonBinaryExpr
  op <- binaryOp
  b <- nonBinaryExpr
  pure $ BinaryExpr op a b
  where
  -- TODO: nested binary expressions
  nonBinaryExpr = choice
    [ boolLiteral
    , numberLiteral
    , groupingExpr p
    , unaryExpr p
    ]

groupingExpr :: P.Parser String Expr -> P.Parser String Expr
groupingExpr p = do
  expr' <- lexer.parens p
  pure $ GroupingExpr expr'

-- Alternatively, could maybe use
-- https://hackage.haskell.org/package/parsec-3.1.15.1/docs/Text-Parsec-Expr.html
expr :: P.Parser String Expr
expr = Lazy.fix \p -> choice
  [ binaryExpr p
  , groupingExpr p
  , unaryExpr p
  , nilLiteral
  , numberLiteral
  , boolLiteral
  ]

readExpr :: String -> Either.Either P.ParseError Expr
readExpr input = P.runParser input expr
