module Lex where

data Token
  = LeftParen    { line :: Int }
  | RightParen   { line :: Int }
  | LeftBrace    { line :: Int }
  | RightBrace   { line :: Int }
  | Comma        { line :: Int }
  | Dot          { line :: Int }
  | Minus        { line :: Int }
  | Plus         { line :: Int }
  | Semicolon    { line :: Int }
  | Slash        { line :: Int }
  | Star         { line :: Int }
  | Bang         { line :: Int }
  | BangEqual    { line :: Int }
  | Equal        { line :: Int }
  | EqualEqual   { line :: Int }
  | Greater      { line :: Int }
  | GreaterEqual { line :: Int }
  | Less         { line :: Int }
  | LessEqual    { line :: Int }

  | Identifier   { line :: Int, lexeme :: String }
  | String       { line :: Int, lexeme :: String, literal :: String }
  | Number       { line :: Int, lexeme :: String, literal :: Number }

  | And          { line :: Int }
  | Class        { line :: Int }
  | Else         { line :: Int }
  | False        { line :: Int }
  | Fun          { line :: Int }
  | For          { line :: Int }
  | If           { line :: Int }
  | Nil          { line :: Int }
  | Or           { line :: Int }
  | Print        { line :: Int }
  | Return       { line :: Int }
  | Super        { line :: Int }
  | This         { line :: Int }
  | True         { line :: Int }
  | Var          { line :: Int }
  | While        { line :: Int }
  | EOF          { line :: Int }
