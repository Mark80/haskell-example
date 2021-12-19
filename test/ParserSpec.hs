module ParserSpec where

import Test.Hspec
import Parser.Combinator

spec :: Spec
spec = do
  describe "Pahrser Combinator" $ do
    it "should parse" $ runParser (charP 'a') "a" `shouldBe` (Right "a", "")
    it "should parse" $ runParser (charP 'a') "aaa" `shouldBe` (Right "a", "aa")
    it "should parse" $ runParser (stringP "abcd") "abcd" `shouldBe` (Right "abcd", "")


