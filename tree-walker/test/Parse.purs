module Test.Parse where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Parse (Expr(..), Literal(..), UnaryOp(..), BinaryOp(..), readExpr)
import Test.Unit (Test, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "comment" do
    test "line comment" do
      parseExprTest "begins" "1 // foo" loxOne
      parseExprTest "ends" "// foo \n 1" loxOne
    test "block comment" do
      parseExprTest "begins and ends" "1 /* foo */ + 1" (BinaryExpr Plus loxOne loxOne)
      parseExprTest "begins and ends with newline" "1 /* foo \n */ + 1" (BinaryExpr Plus loxOne loxOne)
  suite "literal" do
    test "bool" do
      parseExprTest "true" "true" loxTrue
      parseExprTest "false" "false" loxFalse
    test "positive number" do
      parseExprTest "integer" "1" loxOne
      parseExprTest "float" "1.0" loxOne
      parseExprTest "floaty float" "1.1" loxOnePointOne
      parseExprTest "posy integer" "+1" loxOne
      parseExprTest "posy float" "+1.0" loxOne
      parseExprTest "posy floaty float" "+1.1" loxOnePointOne
    test "negative number" do
      parseExprTest "integer" "-1" loxNegativeOne
      parseExprTest "float" "-1.0" loxNegativeOne
      parseExprTest "floaty float" "-1.1" loxNegativeOnePointOne
  suite "compound expressions" do
    test "unary expr" do
      parseExprTest "not true" "!true" (UnaryExpr Bang loxTrue)
      parseExprTest "not true (whitespace)" "! true" (UnaryExpr Bang loxTrue)
    test "binary expr" do
      parseExprTest "addition (no whitespace)" "1+1" (BinaryExpr Plus loxOne loxOne)
      parseExprTest "addition" "1 + 1" (BinaryExpr Plus loxOne loxOne)
      parseExprTest "addition" "1 \n + 1 (extra whitespace)" (BinaryExpr Plus loxOne loxOne)
    -- TODO: nested binary expresssions
    -- parseExprTest "addition three terms" "1 + 1 + 1" (BinaryExpr Plus (BinaryExpr Plus loxOne loxOne) loxOne)
    test "grouping expr" do
      parseExprTest "literal" "(true)" (GroupingExpr loxTrue)
      parseExprTest "unary" "(!true)" (GroupingExpr $ UnaryExpr Bang loxTrue)
      parseExprTest "binary" "(1 + 1)" (GroupingExpr $ BinaryExpr Plus loxOne loxOne)

parseExprTest :: String -> String -> Expr -> Test
parseExprTest msg src expected =
  Assert.assert msg $ case readExpr src of
    Left _err -> false
    Right actual -> actual == expected

loxTrue :: Expr
loxTrue = Literal $ LoxBool true

loxFalse :: Expr
loxFalse = Literal $ LoxBool false

loxOne :: Expr
loxOne = Literal $ LoxNumber 1.0

loxOnePointOne :: Expr
loxOnePointOne = Literal $ LoxNumber 1.1

loxNegativeOne :: Expr
loxNegativeOne = Literal $ LoxNumber (-1.0)

loxNegativeOnePointOne :: Expr
loxNegativeOnePointOne = Literal $ LoxNumber (-1.1)
