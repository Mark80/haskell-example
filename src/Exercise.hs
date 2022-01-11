module Exercise where

and_ :: [Bool] -> Bool
and_ [] = True
and_ l =
  let x : xs = l
   in x && (and_ xs)

repl :: Int -> a -> [a]
repl 0 _ = []
repl n b = [b] ++ repl (n - 1) b
