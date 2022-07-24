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
import Parsing.Expr (Assoc(..), Operator(..), OperatorTable, buildExprParser)
import Parsing.Language as L
import Parsing.String (char)
import Parsing.String.Basic (alphaNum, letter)
import Parsing.Token as T

-- types

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

-- vals
readExpr :: String -> Either.Either P.ParseError Expr
readExpr input = P.runParser input expr

expr :: P.Parser String Expr
expr = Lazy.fix \p -> buildExprParser table (term p)

table :: OperatorTable Identity String Expr
table =
  [ [ -- Unary
      Prefix $ try (lexer.reservedOp "!" $> UnaryExpr Bang)
    , Prefix (lexer.reservedOp "-" $> UnaryExpr Negative)
    ]
  , [ -- Factor
      Infix (lexer.reservedOp "/" $> BinaryExpr Slash) AssocLeft
    , Infix (lexer.reservedOp "*" $> BinaryExpr Star) AssocLeft
    ]
  , [ -- Term
      Infix (lexer.reservedOp "-" $> BinaryExpr Minus) AssocLeft
    , Infix (lexer.reservedOp "+" $> BinaryExpr Plus) AssocLeft
    ]
  , [ -- Comparison
      Infix (lexer.reservedOp ">" $> BinaryExpr Greater) AssocLeft
    , Infix (lexer.reservedOp ">=" $> BinaryExpr GreaterEqual) AssocLeft
    , Infix (lexer.reservedOp "<" $> BinaryExpr Less) AssocLeft
    , Infix (lexer.reservedOp "<=" $> BinaryExpr LessEqual) AssocLeft
    ]
  , [ -- Equality
      Infix (lexer.reservedOp "==" $> BinaryExpr EqualEqual) AssocLeft
    , Infix (lexer.reservedOp "!=" $> BinaryExpr BangEqual) AssocLeft
    ]
  ]

term :: P.Parser String Expr -> P.Parser String Expr
term p = choice
  [ lexer.parens p
  , nilLiteral
  , numberLiteral
  , boolLiteral
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

lexer :: T.GenTokenParser String Identity
lexer = T.makeTokenParser style

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
