module Parser.Combinator where

import Control.Applicative (liftA2, many, (<|>))
import Data.Char (isAlpha, isDigit, isLetter)
import GHC.Base (Alternative, empty)

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

instance Alternative Parser where
  empty = neverP "error"

  Parser ps <|> Parser pd = Parser p
    where
      p s = case ps s of
        (Right a, s') -> (Right a, s')
        (Left _, _) -> case pd s of
          (Right b, s'') -> (Right b, s'')
          (Left err, _) -> (Left err, s)

alwaysP :: a -> Parser a
alwaysP = Parser . (,) . Right

neverP :: String -> Parser a
neverP = Parser . (,) . Left

intP :: Parser Int
intP = fmap read (whileP isDigit)

lexP :: String -> Parser String
lexP s = spaceP *> stringP s <* spaceP

spaceP :: Parser String
spaceP = whileP (flip elem " \n\t")

while1P :: (Char -> Bool) -> Parser String
while1P f = whenP (whileP f) (not . null) "error"

whenP :: Parser a -> (a -> Bool) -> String -> Parser a
whenP p f e = p >>= (\a -> if f a then alwaysP a else neverP e)

notP :: Parser String -> Parser String
notP (Parser pa) = Parser p
  where p s@(x : xs) = case pa s of
                        (Right _, _) ->
                          (Left "error", s)
                        (Left _, _) ->
                          (Right [x], xs)

between :: Parser a -> Parser String -> Parser String -> Parser String
between l e r = l *> (mconcat <$>  many ((e *> r) <|> notP r)) <* r

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
openTag = lexP "<" *> whileP isLetter <* lexP ">"

closeTag :: String -> Parser String
closeTag s = lexP "</" *> stringP s <* lexP ">"

xmlTextP2 :: Parser XML
xmlTextP2 = fmap XMLText ((whileP (/= '<')) >>= (\a -> if (not . null) a then alwaysP a else neverP "error"))

xmlTextP :: Parser XML
xmlTextP = fmap XMLText (while1P (/= '<'))

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

data XML
  = XMLElement String [XML]
  | XMLText String
  deriving (Eq, Show)

xmlP :: Parser XML
xmlP = xmlElementP <|> xmlTextP

xmlElementP :: Parser XML
xmlElementP =
  do
    ot <- openTag
    c <- many xmlP
    ct <- closeTag ot
    return $ XMLElement ct c
