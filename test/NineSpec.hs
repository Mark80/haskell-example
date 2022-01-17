module NineSpec where

import S99
import Test.Hspec

spec :: Spec
spec = do
  describe "99 s" $ do
    it "get last element" $ las [1, 2, 3, 4] `shouldBe` Just 4
    it "get last element" $ las ([] :: [Int]) `shouldBe` Nothing
    it "get penultimate element" $ penultimate [1, 2, 3, 4]  `shouldBe` Just 3
    it "get penultimate element" $ penultimate [1]  `shouldBe` Nothing
    it "get nth element" $ nth 3 [1,2,3,4,5]  `shouldBe` Just 3
    it "get nth element" $ nth 1 [1,2,3,4,5]  `shouldBe` Just 1
    it "get nth element" $ nth 6 [1,2,3,4,5]  `shouldBe` Nothing
    it "reverse list" $ rev2 [1,2,3,4,5]  `shouldBe` [5,4,3,2,1]
    it "is palindrme" $ isPalindrome [1,2,3,2,1]  `shouldBe` True
    it "flatten" $ flatten (List [(Elem 1), List [(Elem 2),(Elem 3),(Elem 4)]])  `shouldBe` [1,2,3,4]
    it "pack" $ pack [1,1,2,3,3,5,5,5,5,5,5,5,8,9,10]   `shouldBe` [1,2,3,5,8,9,10]
