module ParserSpec where

import Test.Hspec
import Parser.Combinator

spec :: Spec
spec = do
  describe "Pahrser Combinator" $ do
    it "should parse" $ runParser (charP 'a') "a" `shouldBe` (Right "a", "")
    it "should parse" $ runParser (charP 'a') "aaa" `shouldBe` (Right "a", "aa")
    it "should parse" $ runParser (stringP "abcd") "abcd" `shouldBe` (Right "abcd", "")
    it "should return an error if the string is not parsable" $ runParser (stringP "accd") "abcd" `shouldBe` (Left "aspetto una carattere uguale a c", "abcd")
    it "should parse integers" $ runParser intP "1234d" `shouldBe` (Right  1234, "d")
    it "should parse reference" $ runParser referenceP "H2" `shouldBe` (Right  ("H", 2), "")
    it "should parse reference 2" $ runParser reference2P "H2" `shouldBe` (Right  ("H", 2), "")
    it "should parse opne tag " $ runParser openTag "<tag>" `shouldBe` (Right  "tag", "")


