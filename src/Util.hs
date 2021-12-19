module Util where

stringLength :: Int -> Int
stringLength = (length . show)

takeN :: Integer -> [a] -> [a]
takeN _ [] = []
takeN 0 _ = []
takeN 1 (x : _) = [x]
takeN n (x:xs) = x : takeN (n -1) xs

