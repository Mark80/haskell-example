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

compress :: Eq a => [a] -> [a]
compress list = case list of
  [] -> []
  [a] -> [a]
  x : y : xs -> if x == y then compress (y : xs) else x : compress (y : xs)
  
  

encode :: Eq a =>  [a] -> [(Int, a)] -> [(Int, a)]
encode list acc = case (list, acc) of
  ([], _) -> rev acc
  (x : xs, []) -> encode xs ([(1, x)])
  (x : xs, y : ys) -> if (snd y) == x then encode xs (((fst y + 1), x) : ys) else encode xs ((1, x) : y : ys)

duplicate :: [a] -> [a]
duplicate list = dup list []
                    where
                      dup [] a = a
                      dup (x : xs) a = a ++ (x : x : (duplicate xs))
