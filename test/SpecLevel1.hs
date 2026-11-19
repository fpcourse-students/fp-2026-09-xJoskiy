module SpecLevel1 where

import Control.Applicative
import Level1
import Parser
import Lexer
import MyIO
import Test.Prelude

-- Сравниваем и результат, и остаток: одинакового AST недостаточно.
parses :: (Eq a, Show a) => MyParser a -> String -> Maybe ([Token], a) -> Test
parses p input expected = TestCase $
  assertEqual input expected (runMyParser p (tokenize input))

tests :: NamedTests
tests = nameTests 1
  [ TestList [testPoint3, testConsole]
  , TestList
      [ TestCase $ assertEqual "присваивание и остаток" (Just ([";"], Assignment "count" 42)) answerAssignment
      , TestCase $ assertEqual "неудача после чтения имени и знака равенства" Nothing answerInvalid
      , TestCase $ assertEqual "меняется результат, сохраняется остаток" (Just (["!"], 5)) answerMapped
      ]
  , TestList [testFunctor, testApplicative, testAlternative]
  , testAssignment
  ]

testPoint3 :: Test
testPoint3 = TestList
  [ lawsToTest $ traversableLaws $ Proxy @Point3
  , propertyToTest "порядок слева направо и сохранение структуры" \s1 s2 s3 ->
      let expected = (s1 ++ s2 ++ s3, Point3 ('!' : s1) ('!' : s2) ('!' : s3)) in
      expected === traverse (\s -> (s, '!' : s)) (Point3 s1 s2 s3)
  , TestCase $ assertEqual "Maybe: успех"
      (Just (Point3 1 2 (3 :: Int))) (sequenceA $ Point3 (Just 1) (Just 2) (Just 3))
  , TestList
      [ TestCase $ assertEqual "Maybe: неудача" (Nothing :: Maybe (Point3 Int)) (sequenceA p)
      | p <- [Point3 Nothing (Just 2) (Just 3), Point3 (Just 1) Nothing (Just 3), Point3 (Just 1) (Just 2) Nothing]
      ]
  , TestCase $ assertEqual "все комбинации списка"
      [Point3 x y z | x <- [1, 4], y <- [2, 5], z <- [3, 6 :: Int]]
      (sequenceA $ Point3 [1, 4] [2, 5] [3, 6])
  ]

deriving stock instance Generic (Point3 a)

instance Arbitrary a => Arbitrary (Point3 a) where
  arbitrary = Point3 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

testFunctor :: Test
testFunctor = TestList
  [ parses (length <$> anyToken) "hello !" (Just (["!"], 5))
  , parses (show <$> natural) "42 ; name" (Just ([";", "name"], "42"))
  , parses (length <$> anyToken) "" Nothing
  , parses ((+ 1) <$> natural) "wrong 42" Nothing
  , propertyToTest "fmap id" \(ts :: [String]) ->
      runMyParser (fmap id anyToken) ts === runMyParser anyToken ts
  , propertyToTest "fmap composition" \(ts :: [String]) ->
      runMyParser (fmap (show . length) anyToken) ts ===
      runMyParser (fmap show (fmap length anyToken)) ts
  ]

testApplicative :: Test
testApplicative = TestList
  [ parses (pure (42 :: Int)) "a b" (Just (["a", "b"], 42))
  , parses (pure ()) "" (Just ([], ()))
  , parses ((,) <$> token "a" <*> token "b") "a b !" (Just (["!"], ("a", "b")))
  , parses ((,) <$> token "a" <*> token "b") "x b" Nothing
  , parses ((,) <$> token "a" <*> token "b") "a x" Nothing
  , parses ((,) <$> token "a" <*> token "b") "a" Nothing
  , parses ((,) <$> token "a" <*> token "b") "" Nothing
  , parses ((+) <$> natural <*> natural) "12 30 7" (Just (["7"], 42))
  , parses (token "a" *> natural <* token ";") "a 42 ; !" (Just (["!"], 42))
  , propertyToTest "pure id <*> p" \(ts :: [String]) ->
      runMyParser (pure id <*> anyToken) ts === runMyParser anyToken ts
  , propertyToTest "pure f <*> pure x не читает ввод" \(n :: Int) (ts :: [String]) ->
      runMyParser (pure (+ 1) <*> pure n) ts === Just (ts, n + 1)
  ]

testAlternative :: Test
testAlternative = TestList
  [ parses (empty :: MyParser Int) "" Nothing
  , parses (empty :: MyParser Int) "1" Nothing
  , parses (token "a" <|> token "b") "a !" (Just (["!"], "a"))
  , parses (token "a" <|> token "b") "b !" (Just (["!"], "b"))
  , parses (token "a" <|> token "b") "c" Nothing
  , parses (token "a" <|> token "b") "" Nothing
  , parses ((1 <$ token "a") <|> (2 <$ token "a")) "a !" (Just (["!"], 1 :: Int))
  , parses ((token "a" *> token "b") <|> token "a") "a c" (Just (["c"], "a"))
  , parses ((token "a" *> token "b" *> token "c") <|> token "a") "a b x" (Just (["b", "x"], "a"))
  , parses (many (token "a")) "a a b" (Just (["b"], ["a", "a"]))
  , parses (many (token "a")) "b" (Just (["b"], []))
  , parses (some (token "a")) "b" Nothing
  , propertyToTest "empty -- нейтральный элемент слева и справа" \(ts :: [String]) ->
      (runMyParser (empty <|> anyToken) ts, runMyParser (anyToken <|> empty) ts) ===
      (runMyParser anyToken ts, runMyParser anyToken ts)
  ]

testAssignment :: Test
testAssignment = TestList
  [ parses parseAssignment "count = 42 ;" (Just ([";"], Assignment "count" 42))
  , parses parseAssignment "A=0" (Just ([], Assignment "A" 0))
  , parses parseAssignment "limit = 007 next = 2" (Just (["next", "=", "2"], Assignment "limit" 7))
  , parses parseAssignments " count = 42; " (Just ([], [Assignment "count" 42]))
  , parses parseAssignments "count=42; limit = 10;"
      (Just ([], [Assignment "count" 42, Assignment "limit" 10]))
  , parses parseAssignments "x=1; x=2; X=0;"
      (Just ([], [Assignment "x" 1, Assignment "x" 2, Assignment "X" 0]))
  , parses parseAssignments " \n " (Just ([], []))
  , TestList [parses parseAssignments s Nothing | s <-
      ["count = 42", "count = 42; limit =", "count = 42; limit = 10", "count = 42 other = 3;"
      , "count = 42;;", ";", "count = 42; garbage", "42 = 3;", "x = -2;", "x = 2a;"]]
  , TestList [parses parseAssignment s Nothing | s <- ["", "count", "count =", "= 42", "count 42", "count == 42", "42 = 3", "n1 = 3", "x = -2", "x = 2a", "x = name"]]
  , propertyToTest "ручной разбор и комбинаторы: результат и остаток" $
      forAll (listOf1 $ elements ['a' .. 'z']) \name ->
      \(NonNegative (n :: Int)) (rest :: [String]) ->
        let input = name : "=" : show n : rest in
        let expected = Just (rest, Assignment name n) in
        (runMyParser parseAssignmentManual input, runMyParser parseAssignment input) ===
        (expected, expected)
  , TestList
      [ TestCase $ assertEqual ("неудача ручного разбора и комбинаторов: " ++ s)
          (Nothing, Nothing)
          (runMyParser parseAssignmentManual (tokenize s), runMyParser parseAssignment (tokenize s))
      | s <- ["", "42 = 3", "count 42", "count =", "count = oops", "count = -2"]
      ]
  ]

testConsole :: Test
testConsole = TestList
  [ TestCase $ assertEqual "printSum" (unsafeWorld [] [3, 4, 5, 1, 2, 3, 4, 6, 8]) $
      execUnsafeMyIO (printSum (Point3 3 4 5) (Point3 1 2 3)) emptyWorld
  , propertyToTest "сумма произвольных точек" \x y z a b c ->
      unsafeWorld [99] [7, x, y, z, a, b, c, x + a, y + b, z + c] ===
      execUnsafeMyIO (printSum (Point3 x y z) (Point3 a b c)) (unsafeWorld [99] [7])
  ]
