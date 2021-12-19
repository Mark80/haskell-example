module Parser.Combinator where

import Test.Hspec

newtype Parser a = NewParser (String -> (Either String a, String))

charP :: Char -> Parser String
charP c = NewParser p
  where
    p "" = (Left "aspetto una stringa non vuota", "")
    p (x : xs)
      | x == c = (Right [x], xs)
      | otherwise = (Left "aspetto una carattere uguale a c", "")

instance Semigroup a => Semigroup (Parser a) where
  NewParser sx <> NewParser dx = NewParser p
    where
      p s = case sx s of
        (Right x, s') ->
          case dx s' of
            (Right y, s'') ->
              (Right $ x <> y, s'')
            (Left e, _) -> (Left e, s)
        (Left e, _) ->
          (Left e, s)

instance Monoid a => Monoid (Parser a) where
  mempty = NewParser (\s -> (Right mempty, ""))

stringP :: String -> Parser String
stringP "" = mempty
stringP (h : t) = charP h <> stringP t

runParser :: Parser a -> String -> (Either String a, String)
runParser (NewParser f) = f

execParser :: Parser a -> String -> Either String a
execParser p s = (fst . runParser p) s

tests :: IO ()
tests = hspec $ do
  describe "Pahrser Combinator" $ do
    it "should parse" $ runParser (charP 'a') "a" `shouldBe` (Right "a", "")
    it "should parse" $ runParser (charP 'a') "aaa" `shouldBe` (Right "a", "aa")
    it "should parse" $ runParser (stringP "abcd") "abcd" `shouldBe` (Right "abcd", "")
