module Test.Lex where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Lex (Expr(..), Literal(..), Op(..), readExpr)
import Test.Unit (Test, suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

parseExprTest :: String -> String -> Expr -> Test
parseExprTest msg src expected =
  timeout 100 $ Assert.assert msg $ case readExpr src of
    Left _err -> false
    Right actual -> actual == expected

loxTrue :: Expr
loxTrue = Literal $ LoxBool true

loxFalse :: Expr
loxFalse = Literal $ LoxBool false

loxOne :: Expr
loxOne = Literal $ LoxNumber 1.0

main :: Effect Unit
main = runTest do
  suite "literal" do
    test "bool" do
      parseExprTest "true" "true" loxTrue
      parseExprTest "false" "false" loxFalse
    test "number" do
      parseExprTest "integer" "1" loxOne
      parseExprTest "float" "1.0" loxOne
  suite "compound literal" do
    test "unary expr" do
      parseExprTest "negative int" "-1" (UnaryExpr Minus loxOne)
      parseExprTest "negative float" "-1.0" (UnaryExpr Minus loxOne)
      parseExprTest "not true" "!true" (UnaryExpr Bang loxTrue)
      parseExprTest "not true (whitespace)" "! true" (UnaryExpr Bang loxTrue)
    test "binary expr" do
      parseExprTest "addition" "1 + 1" (BinaryExpr loxOne Plus loxOne)
    test "grouping expr" do
      parseExprTest "literal" "(true)" (GroupingExpr loxTrue)
      parseExprTest "unary" "(-1)" (GroupingExpr $ UnaryExpr Minus loxOne)
      parseExprTest "binary" "(1 + 1)" (GroupingExpr $ BinaryExpr loxOne Plus loxOne)
