module Parser.Combinator where

import Control.Applicative (liftA2)
import Data.Char (isAlpha, isDigit, isLetter)

newtype Parser a = Parser (String -> (Either String a, String))

charP :: Char -> Parser String
charP c = Parser p
  where
    p "" = (Left "aspetto una stringa non vuota", "")
    p (x : xs)
      | x == c = (Right [x], xs)
      | otherwise = (Left ("aspetto una carattere uguale a:" ++ show c), "")

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

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
              (Right b, s'') -> (Right $ f b, s'')
              (Left err, _) -> (Left err, s)
          (Left err, _) -> (Left err, s)

instance Monad Parser where
  (Parser pa) >>= f = Parser p
    where
      p s =
        case pa s of
          (Right a, s') -> runParser (f a) s'
          (Left err, _) -> (Left err, s)

instance Monoid a => Monoid (Parser a) where
  mempty = alwaysP mempty

alwaysP :: a -> Parser a
alwaysP = Parser . (,) . Right

intP :: Parser Int
intP = fmap read (whileP isDigit)

whileP :: (Char -> Bool) -> Parser String
whileP f = Parser p
  where
    p s =
      let (l, r) = span f s
       in (Right l, r)

stringP :: String -> Parser String
stringP = foldMap charP

reference2P :: Parser (String, Int)
reference2P =
  let px = (whileP isAlpha)
      dx = intP
   in (fmap (,) px) <*> dx

openTag :: Parser String
openTag = charP '<' *> whileP isLetter <* charP '>'

closeTag :: String -> Parser String
closeTag s = stringP "</" *> stringP s <* charP '>'

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

data XML = XMLElement String [XML]
  deriving (Eq, Show)

xmlP2 :: Parser XML
xmlP2 = XMLElement <$> (openTag >>= closeTag) <*> pure []

xmlP :: Parser XML
xmlP = do
  ot <- openTag
  ct <- closeTag ot
  return $ XMLElement ct []
