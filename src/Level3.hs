{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Челлендж: язык арифметических выражений и транспонирование.
module Level3 where

import Control.Applicative hiding (Const (..))
import Level1
import Parser
import Level2 (BinOp (..), Expr (..), parseConst, parseVar, inParens)
import MetaUtils


-- 3.1. Выражения с приоритетами.
-- Через комбинаторы соберите парсер выражений со скобками и операциями +, -, *.
-- Сначала дополните chainl1: прочитать operand и ноль или более пар
-- (operator, operand), применить полученные функции слева: 8 - 3 - 1 = (8 - 3) - 1.
-- Первый операнд обязателен; неудавшуюся пару оставьте непрочитанной.
-- Оба переданных парсера при успехе должны читать хотя бы один токен.
-- runMyParser (chainl1 natural ((-) <$ token "-")) (tokenize "8 - 3 -")
--   == Just (["-"], 5)

chainl1 :: MyParser a -> MyParser (a -> a -> a) -> MyParser a
chainl1 operand operator = assemble <$> operand <*> many (todo "chain step")
  where
    assemble :: a -> [(a -> a -> a, a)] -> a
    assemble = todo "assemble"

-- Затем используйте chainl1 и inParens для грамматики (| -- выбор, { ... } -- повторение):
--   expr    = product { ("+" | "-") product }
--   product = atom { "*" atom }
--   atom    = natural | identifier | "(" expr ")"
-- Приоритет * выше + и -; все операции ассоциативны влево, унарного минуса нет.
-- parseExpr читает префикс, parseExprAll -- весь ввод:
--   "1 +" даёт Just (["+"], Const 1) и Nothing соответственно.
-- Не начинайте expr с вызова самого себя: без чтения токена это бесконечная
-- левая рекурсия. Вызов expr внутри atom идёт после чтения "(".

parseAtom :: MyParser Expr
parseAtom = todo "parseAtom"

parseProduct :: MyParser Expr
parseProduct = todo "parseProduct"

parseExpr :: MyParser Expr
parseExpr = todo "parseExpr"

parseExprAll :: MyParser Expr
parseExprAll = todo "parseExprAll"


-- 3.2. Транспонирование.
-- Транспонируйте конечную прямоугольную матрицу через Applicative и Traversable.
-- Подсказка: сравните Applicative для [] и ZipList.
-- transpose [[1, 2], [3, 4]] == [[1, 3], [2, 4]].
-- Для [] и [[], []] результат -- [].

transpose :: [[a]] -> [[a]]
transpose = todo "transpose"
