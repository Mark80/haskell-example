module Lib
  ( removeOdd,
    removeOdd2,
    removeOdd3,
    removeOdd4,
    stuff,
    infinite_list,
    compose
  )
where

removeOdd :: [Int] -> [Int]
removeOdd nums
  | null nums = []
  | mod (head nums) 2 == 0 = head nums : removeOdd (tail nums)
  | otherwise = removeOdd (tail nums)

removeOdd2 :: [Int] -> [Int]
removeOdd2 nums =
  if null nums
    then []
    else
      if (mod (head nums) 2) == 0
        then (head nums) : removeOdd2 (tail nums)
        else removeOdd2 (tail nums)

removeOdd3 :: [Int] -> [Int]
removeOdd3 [] = []
removeOdd3 (head : tail) =
  let mod2 = (mod head 2)
   in if mod2 == 0
        then head : removeOdd3 tail
        else removeOdd3 tail

removeOdd4 :: [Int] -> [Int]
removeOdd4 nums = case nums of
  [] -> []
  x : xs -> case mod x 2 of
    0 -> x : removeOdd4 xs
    _ -> removeOdd4 xs

stuff = removeOdd4 nums
  where
    nums = [12, 3, 4, 5, 6]

isEven x = (mod x 2) == 0 

removeOdd5 :: [Int] -> [Int]
removeOdd5 nums = filter isEven nums 

infinite_list :: Num t => t -> [t]
infinite_list n  = n : (infinite_list (n + 1))

propieta_associativa :: (Eq a, Num a) => a -> a -> a -> Bool
propieta_associativa a b c = (a + b) + c == a + (b + c)

compose :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
compose f g x = f (g x)
