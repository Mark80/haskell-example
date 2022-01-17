module S99 where

las :: [a] -> Maybe a
las list = case list of
  [] -> Nothing
  [a] -> Just a
  _ : xs -> las xs

penultimate :: [a] -> Maybe a
penultimate list = case list of
  [] -> Nothing
  [a, _] -> Just a
  _ : xs -> penultimate xs

nth :: Int -> [a] -> Maybe a
nth n list = case (n, list) of
  (_, []) -> Nothing
  (1, x : _) -> Just x
  (r, _ : xs) -> nth (r - 1) xs

rev :: [a] -> [a]
rev list =
  case list of
    [] -> []
    [a] -> [a]
    (x : xs) -> rev xs ++ [x]

rev2 :: [a] -> [a]
rev2 l = re l []
  where
    re [] a = a
    re (x : xs) a = re xs (x : a)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == rev l

data NestedList a
  = Elem a
  | List [NestedList a]

flatten :: NestedList a -> [a]
flatten l = case l of
  Elem x -> [x]
  List (x : xs) -> flatten x ++ flatten (List xs)
  List [] -> []


pack :: Eq a => [a] -> [a]
pack list = case list of 
               [] -> []
               [a] -> [a]
               x:y:xs -> if x == y then pack (y:xs) else x: pack (y:xs)
                  