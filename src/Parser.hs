-- | Готовые примитивы парсера и пример ручного разбора.
module Parser where

import Data.Char qualified as Char
import Lexer (Token)

-- Лексер удаляет пробелы и выделяет токены: tokenize "name = 42" == ["name", "=", "42"].
-- Парсер возвращает Just (остаток, результат) при успехе и Nothing при неудаче.

newtype MyParser a = MyParser
  { runMyParser :: [Token] -> Maybe ([Token], a) }

-- Прочитать первый токен, если он удовлетворяет предикату.
satisfy :: (Token -> Bool) -> MyParser Token
satisfy p = MyParser \case
  t:ts' | p t -> Just (ts', t)
  _ -> Nothing

token :: Token -> MyParser Token
token t = satisfy (== t)

anyToken :: MyParser Token
anyToken = satisfy (const True)

-- Имя -- непустая последовательность латинских букв (без цифр и подчёркиваний).
identifier :: MyParser String
identifier = satisfy \s -> not (null s) && all isLetter s
  where
    isLetter c = Char.isAsciiLower c || Char.isAsciiUpper c

-- Успех только на пустом вводе. Ничего не читает.
eof :: MyParser ()
eof = MyParser \case
  [] -> Just ([], ())
  _ -> Nothing

-- Числовой токен: непустая последовательность цифр 0..9.
-- Во входных данных числа помещаются в Int.
decimal :: Token -> Bool
decimal s = not (null s) && all Char.isDigit s

data Assignment = Assignment String Int
  deriving (Show, Eq)

-- Готовый разбор name = 42: проверяем успех каждого шага и передаём остаток.
-- Работает до заполнения инстансов MyParser.
parseAssignmentManual :: MyParser Assignment
parseAssignmentManual = MyParser \ts -> case runMyParser identifier ts of
  Nothing -> Nothing
  Just (rest1, name) -> case runMyParser (token "=") rest1 of
    Nothing -> Nothing
    Just (rest2, _) -> case runMyParser (satisfy decimal) rest2 of
      Nothing -> Nothing
      Just (rest3, digits) -> Just (rest3, Assignment name (read digits))

-- Повторяющиеся case обобщим в комбинаторы и перепишем этот разбор в 1.4.
