module SpecLevel2 where

import Level1
import Parser
import Level2
import SpecLevel1 (parses)
import Test.Prelude

tests :: NamedTests
tests = nameTests 2
  [ testTree
  , testPolish
  , testChain
  , testParens
  ]

testTree :: Test
testTree = TestList
  [ lawsToTest $ traversableLaws $ Proxy @InorderTraversal
  , TestCase $ assertEqual "порядок и форма" ("146235", InorderTraversal tree)
      (traverse (\x -> (show x, x)) $ InorderTraversal tree)
  , TestCase $ assertEqual "Maybe: неудача"
      (Nothing :: Maybe (InorderTraversal Int))
      (sequenceA $ InorderTraversal $ Node1 (Node1 Leaf1 (Just 1) Leaf1) Nothing Leaf1)
  , TestCase $ assertEqual "пустое дерево"
      (Just (InorderTraversal Leaf1) :: Maybe (InorderTraversal Int))
      (traverse (const Nothing) $ InorderTraversal Leaf1)
  , TestCase $ assertEqual "порядок комбинаций списка"
      [InorderTraversal (Node1 (Node1 Leaf1 l Leaf1) x (Node1 Leaf1 r Leaf1))
      | l <- [1, 2], x <- [3, 4], r <- [5, 6 :: Int]]
      (sequenceA $ InorderTraversal $ Node1 (Node1 Leaf1 [1, 2] Leaf1) [3, 4] (Node1 Leaf1 [5, 6] Leaf1))
  ]
  where
    tree = Node1 (Node1 Leaf1 1 Leaf1) 4
      (Node1 Leaf1 6 (Node1 (Node1 Leaf1 2 Leaf1) 3 (Node1 Leaf1 (5 :: Int) Leaf1)))

deriving via GenericArbitrary (Tree1 a) instance Arbitrary a => Arbitrary (Tree1 a)

deriving newtype instance Arbitrary a => Arbitrary (InorderTraversal a)

testPolish :: Test
testPolish = TestList
  [ parses parsePolish "+ 2 3 q" (Just (["q"], BinOp Plus (Const 2) (Const 3)))
  , parses parsePolish "1" (Just ([], Const 1))
  , parses parsePolish "+ 2 - 3 5" (Just ([], BinOp Plus (Const 2) (BinOp Minus (Const 3) (Const 5))))
  , parses parsePolish "+ * 2 3 5" (Just ([], BinOp Plus (BinOp Mult (Const 2) (Const 3)) (Const 5)))
  , parses parsePolish "- 8 3 1" (Just (["1"], BinOp Minus (Const 8) (Const 3)))
  , TestList [parses parsePolish s Nothing | s <- ["", "+", "+ 1", "+ + 1", "+ 1 x", "(1)", "name"]]
  ]

testChain :: Test
testChain = TestList
  [ TestCase $ assertEqual "пустая цепочка" (Var "x") (assembleChain (Var "x") [])
  , TestCase $ assertEqual "левая ассоциативность"
      (BinOp Minus (BinOp Minus (Const 8) (Const 3)) (Const 1))
      (assembleChain (Const 8) [(Minus, Const 3), (Minus, Const 1)])
  , TestCase $ assertEqual "приоритетов здесь нет"
      (BinOp Mult (BinOp Plus (Const 1) (Const 2)) (Var "x"))
      (assembleChain (Const 1) [(Plus, Const 2), (Mult, Var "x")])
  ]

testParens :: Test
testParens = TestList
  [ parses (inParens natural) "(42) !" (Just (["!"], 42))
  , parses (inParens $ token "a") "(a)" (Just ([], "a"))
  , TestList [parses (inParens natural) s Nothing | s <- ["42", "(42", "42)", "()", "(x)"]]
  , parses parseParensExpr "((name + 1) * 2)"
      (Just ([], BinOp Mult (BinOp Plus (Var "name") (Const 1)) (Const 2)))
  , parses parseParensExpr "(((42)))" (Just ([], Const 42))
  , parses parseParensExpr "((name + (((1)))))" (Just ([], BinOp Plus (Var "name") (Const 1)))
  , parses parseParensExpr "(8 - (3 - 1)) !"
      (Just (["!"], BinOp Minus (Const 8) (BinOp Minus (Const 3) (Const 1))))
  , parses parseParensExpr "1 + 2" (Just (["+", "2"], Const 1))
  , TestList [parses parseParensExpr s Nothing | s <- ["", "()", "(1 +)", "(1 + 2", "(1 + 2 * 3)", "(x y)"]]
  ]
