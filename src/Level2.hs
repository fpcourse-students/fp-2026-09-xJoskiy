-- | К сессии: короткие задачи на обходы, парсеры и ранее изученные темы.
module Level2 where

import Control.Applicative hiding (Const (..))
import Data.Coerce
import Level1
import Parser
import MetaUtils


-- 2.1. Другой порядок обхода.
-- Реализуйте Traversable InorderTraversal с сохранением формы дерева:
-- левое поддерево, вершина, правое поддерево. Здесь порядок [1, 4, 6, 2, 3, 5]:
--
--       4
--      / \
--     1   6
--          \
--           3
--          / \
--         2   5
--
-- Без deriving Traversable.

data Tree1 a = Leaf1 | Node1 (Tree1 a) a (Tree1 a)
  deriving (Eq, Functor)

newtype InorderTraversal a = InorderTraversal (Tree1 a)
  deriving newtype (Eq, Functor)

instance Foldable InorderTraversal where
  foldMap f (InorderTraversal tree) = go tree
    where
      go = \case
        Leaf1 -> mempty
        Node1 l x r -> go l <> f x <> go r

todoImpl ''Traversable ''InorderTraversal

-- Красивая печать дерева для примеров в GHCi.
deriving stock instance Generic (Tree1 a)
instance Out a => Out (Tree1 a)
instance Show a => Show (Tree1 a) where
  show = pretty . coerce @(Tree1 a) @(Tree1 (OutShow a))
deriving newtype instance Show a => Show (InorderTraversal a)

data BinOp = Plus | Minus | Mult
  deriving Eq

data Expr = Const Int | Var String | BinOp BinOp Expr Expr
  deriving Eq

instance Show BinOp where
  show = \case
    Plus -> "+"
    Minus -> "-"
    Mult -> "*"

instance Show Expr where
  show = \case
    Const n -> show n
    Var name -> name
    BinOp op l r -> "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

parseConst :: MyParser Expr
parseConst = Const <$> natural

parseVar :: MyParser Expr
parseVar = Var <$> identifier

parseBinOp :: MyParser BinOp
parseBinOp = Plus <$ token "+" <|> Minus <$ token "-" <|> Mult <$ token "*"


-- 2.2. Польская запись.
-- Через комбинаторы разберите префикс выражения по грамматике:
--   polish = natural | operator polish polish, где operator -- +, - или *.
-- runMyParser parsePolish (tokenize "+ 2 - 3 5 q")
--   == Just (["q"], BinOp Plus (Const 2) (BinOp Minus (Const 3) (Const 5)))
-- runMyParser parsePolish (tokenize "+ + 1") == Nothing

parsePolish :: MyParser Expr
parsePolish = todo "parsePolish"


-- 2.3. Цепочка операций.
-- Соберите AST из первого выражения и пар (оператор, правый операнд).
-- Группируйте слева, без учёта приоритетов:
-- assembleChain (Const 8) [(Minus, Const 3), (Minus, Const 1)]
--   == BinOp Minus (BinOp Minus (Const 8) (Const 3)) (Const 1)

assembleChain :: Expr -> [(BinOp, Expr)] -> Expr
assembleChain = todo "assembleChain"


-- 2.4. Явные скобки.
-- Через комбинаторы напишите inParens p: прочитать "(", p, ")" и вернуть результат p.
-- Затем разберите префикс выражения по грамматике:
--   parensExpr = natural | identifier | "(" parensExpr ")"
--              | "(" parensExpr operator parensExpr ")", где operator -- +, - или *.
-- Каждая бинарная операция требует скобок; лишние скобки допустимы.
-- runMyParser parseParensExpr (tokenize "((name + 1) * 2)")
--   == Just ([], BinOp Mult (BinOp Plus (Var "name") (Const 1)) (Const 2))
-- На "1 + 2" результат Just (["+", "2"], Const 1), на "(1 +)" -- Nothing.

inParens :: MyParser a -> MyParser a
inParens = todo "inParens"

parseParensExpr :: MyParser Expr
parseParensExpr = todo "parseParensExpr"
