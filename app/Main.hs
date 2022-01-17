module Main where

main :: IO ()
main = do
  content <- readFile "/Users/marco.tosini/projects/haskell-example/src/Exercise.hs"
  saluto <- return "Hello"
  putStrLn content 
  putStrLn saluto
