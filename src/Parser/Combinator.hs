module Parser.Combinator where

import Data.Char (isAlpha, isDigit)

newtype Parser a = Parser (String -> (Either String a, String))

charP :: Char -> Parser String
charP c = Parser p
  where
    p "" = (Left "aspetto una stringa non vuota", "")
    p (x : xs)
      | x == c = (Right [x], xs)
      | otherwise = (Left "aspetto una carattere uguale a c", "")

instance Semigroup a => Semigroup (Parser a) where
  Parser sx <> Parser dx = Parser p
    where
      p s = case sx s of
        (Right x, s') ->
          case dx s' of
            (Right y, s'') ->
              (Right $ x <> y, s'')
            (Left e, _) -> (Left e, s)
        (Left e, _) ->
          (Left e, s)

instance Functor Parser where
  fmap f (Parser pa) = Parser p
    where
      p s = case pa s of
        (Right a, s') ->
          (Right $ f a, s')
        (Left e, _) ->
          (Left e, s)

instance Applicative Parser where
  pure = Parser . (,) . Right

  Parser ps <*> Parser pd = Parser p
    where
      p s =
        case ps s of
          (Right f, s') ->
            case pd s' of
              (Right b, s'') ->
                (Right $ f b, s'')
              (Left err, _) ->
                (Left err, s)
          (Left err, _) ->
            (Left err, s)

instance Monoid a => Monoid (Parser a) where
  mempty = Parser (\_ -> (Right mempty, ""))

intP :: Parser Int
intP = fmap read (whileP isDigit)

whileP :: (Char -> Bool) -> Parser String
whileP f = Parser p
  where
    p s =
      let (l, r) = span f s
       in (Right l, r)

stringP :: String -> Parser String
stringP "" = mempty
stringP (h : t) = charP h <> stringP t

reference2P :: Parser (String, Int)
reference2P =
  let px = (whileP isAlpha)
      dx = intP
   in ((,) <$> px) <*> dx

referenceP :: Parser (String, Int)
referenceP = Parser p
  where
    p s =
      let Parser px = (whileP isAlpha)
          Parser dx = intP
       in case px s of
            (Right a, s') ->
              case dx s' of
                (Right b, s'') ->
                  (Right (a, b), s'')
                (Left err, _) ->
                  (Left err, s)
            (Left err, _) ->
              (Left err, s)

runParser :: Parser a -> String -> (Either String a, String)
runParser (Parser f) = f

execParser :: Parser a -> String -> Either String a
execParser p s = (fst . runParser p) s
