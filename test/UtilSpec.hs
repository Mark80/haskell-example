module UtilSpec where

import Test.Hspec
import Util

spec :: Spec
spec =  do
  describe "stringLength works" $ do
    it "should works" $ stringLength 123 `shouldBe` 3
  describe "take n" $ do
    it "should woks" $ takeN 3 [1,2,3,4,5] `shouldBe` [1,2,3]
    it "should woks with empty list" $ takeN 3 ([] :: [Int]) `shouldBe` []
    it "should woks with n greater than size of list" $ takeN 6 [1,2,3,4] `shouldBe` [1,2,3, 4]
    it "should woks with n greater than size of list 2" $ takeN 2 [1,2,3,4] `shouldBe` [1,2]
    it "should woks with n greater than size of list 3" $ takeN 0 [1,2,3,4] `shouldBe` []