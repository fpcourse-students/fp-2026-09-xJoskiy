module SpecLevel3 where

import Control.Applicative hiding (Const (..))
import Level1
import Parser
import Level2 (BinOp (..), Expr (..))
import Level3
import SpecLevel1 (parses)
import Test.Prelude

tests :: NamedTests
tests = nameTests 3
  [ TestList [testChainl1, testExpr]
  , testTranspose
  ]

testChainl1 :: Test
testChainl1 = TestList
  [ parses subtractChain "8 - 3 - 1 !" (Just (["!"], 4))
  , parses subtractChain "42" (Just ([], 42))
  , parses subtractChain "8 - 3 -" (Just (["-"], 5))
  , parses subtractChain "8 - x" (Just (["-", "x"], 8))
  , parses subtractChain "" Nothing
  , parses subtractChain "- 3" Nothing
  , parses (chainl1 anyToken ((++) <$ token "+")) "ab + cd + ef !" (Just (["!"], "abcdef"))
  , propertyToTest "левая свёртка цепочки вычитаний" \(NonEmpty (ns :: [NonNegative Int])) ->
      let values = map ((`mod` 1000) . getNonNegative) ns in
      let input = case values of
            [] -> []
            x:xs -> show x : concatMap (\n -> ["-", show n]) xs in
      case values of
        [] -> counterexample "генератор должен давать непустой список" False
        x:xs -> runMyParser subtractChain input === Just ([], foldl (-) x xs)
  ]
  where
    subtractChain = chainl1 natural ((-) <$ token "-")

testExpr :: Test
testExpr = TestList
  [ parses parseAtom "name + 1" (Just (["+", "1"], Var "name"))
  , parses parseAtom "(1 + 2) * 3" (Just (["*", "3"], BinOp Plus (Const 1) (Const 2)))
  , parses parseProduct "2 * 3 + 4" (Just (["+", "4"], BinOp Mult (Const 2) (Const 3)))
  , parses parseProduct "2 *" (Just (["*"], Const 2))
  , parses parseExpr "1 +" (Just (["+"], Const 1))
  , parses parseExpr "1 + 2 *" (Just (["*"], BinOp Plus (Const 1) (Const 2)))
  , parses parseExpr "1 + 2 )" (Just ([")"], BinOp Plus (Const 1) (Const 2)))
  , parses parseExprAll "42" (Just ([], Const 42))
  , parses parseExprAll " (  42 ) " (Just ([], Const 42))
  , parses parseExprAll "x*y" (Just ([], BinOp Mult (Var "x") (Var "y")))
  , parses parseExprAll "1 + 2 * ab - x"
      (Just ([], BinOp Minus (BinOp Plus (Const 1) (BinOp Mult (Const 2) (Var "ab"))) (Var "x")))
  , parses parseExprAll "8-3-1" (Just ([], BinOp Minus (BinOp Minus (Const 8) (Const 3)) (Const 1)))
  , parses parseExprAll "2*3*4" (Just ([], BinOp Mult (BinOp Mult (Const 2) (Const 3)) (Const 4)))
  , parses parseExprAll "(1+2)*(3-4)"
      (Just ([], BinOp Mult (BinOp Plus (Const 1) (Const 2)) (BinOp Minus (Const 3) (Const 4))))
  , parses parseExprAll "9 + (6+(((((1+7*2)*3)*5))))"
      (Just ([], BinOp Plus (Const 9) (BinOp Plus (Const 6)
        (BinOp Mult (BinOp Mult (BinOp Plus (Const 1) (BinOp Mult (Const 7) (Const 2))) (Const 3)) (Const 5)))))
  , TestList [parses parseExprAll s Nothing | s <- ["", "()", "1 +", "1 + 2 *", "(1+2", "1+2)", "1 2", "1 ** 2", "* 2", "-1", "x_1", "1x", "1 + (2 * )"]]
  ]

testTranspose :: Test
testTranspose = TestList
  [ TestCase $ assertEqual "квадратная матрица"
      [[1, 2, 3], [4, 5, 6], [7, 8, 9 :: Int]] (transpose [[1, 4, 7], [2, 5, 8], [3, 6, 9]])
  , TestCase $ assertEqual "одноэлементная матрица" [[1 :: Int]] (transpose [[1]])
  , TestCase $ assertEqual "пустая матрица" ([] :: [[Int]]) (transpose [])
  , TestCase $ assertEqual "пустые строки" ([] :: [[Int]]) (transpose [[], []])
  , TestCase $ assertEqual "прямоугольная матрица"
      [[1, 3, 5], [2, 4, 6 :: Int]] (transpose [[1, 2], [3, 4], [5, 6]])
  , TestCase $ assertEqual "одна строка" [[1], [2], [3 :: Int]] (transpose [[1, 2, 3]])
  , TestCase $ assertEqual "один столбец" [[1, 2, 3 :: Int]] (transpose [[1], [2], [3]])
  ]
