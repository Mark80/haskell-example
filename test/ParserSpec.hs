module ParserSpec where

import Test.Hspec
import Parser.Combinator
import Data.Char (isLetter)

spec :: Spec
spec = do
  describe "Pahrser Combinator" $ do
    it "should parse" $ runParser (charP 'a') "a" `shouldBe` (Right "a", "")
    it "should parse" $ runParser (charP 'a') "aaa" `shouldBe` (Right "a", "aa")
    it "should parse" $ runParser (stringP "abcd") "abcd" `shouldBe` (Right "abcd", "")
    it "should return an error if the string is not parsable" $ runParser (stringP "accd") "abcd" `shouldBe` (Left "aspetto una carattere uguale a:'c'", "abcd")
    it "should parse integers" $ runParser intP "1234d" `shouldBe` (Right  1234, "d")
    it "should parse reference" $ runParser referenceP "H2" `shouldBe` (Right  ("H", 2), "")
    it "should parse reference 2" $ runParser reference2P "H2" `shouldBe` (Right  ("H", 2), "")
    it "should parse open tag " $ runParser openTag "<tag>" `shouldBe` (Right  ("tag", []), "")
    it "should parse close tag " $ runParser (closeTag "foo") "</foo>" `shouldBe` (Right  "foo", "")
    it "should parse xml" $ runParser (whileP isLetter) ""  `shouldBe` (Right (""),"")
    it "should parse xml" $ runParser xmlElementP "<tag></tag>"  `shouldBe` (Right  (XMLElement "tag" [] []), "")
    it "should parse xml" $ runParser xmlElementP "<tag>ciccio</tag>"  `shouldBe` (Right  (XMLElement "tag" [] [XMLText "ciccio"]), "")
    it "should parse space" $ runParser spaceP "    ciccio"  `shouldBe` (Right "    ", "ciccio")
    it "should parse xml with space" $ runParser xmlElementP "< tag >ciccio</tag >"  `shouldBe` (Right  (XMLElement "tag" [] [XMLText "ciccio"]), "")
    it "should parse nested xml" $ runParser xmlElementP "<tag><foo></foo></tag>"  `shouldBe` (Right  (XMLElement "tag" [] [XMLElement "foo" [] []]), "")
    it "should parse between" $ runParser (between (stringP "@") (stringP "//") (stringP "@" )) "@mark.tosini//@gmail.com@"   `shouldBe` (Right  "mark.tosini@gmail.com", "")
    it "should parse attribute name" $ runParser nameP "foo=\"bar\""   `shouldBe` (Right  "foo", "=\"bar\"")
    it "should parse attribute name" $ runParser valueP "=\"bar\""   `shouldBe` (Right  "bar", "")
    it "should parse attribute" $ runParser attributeP "foo=\"bar\""   `shouldBe` (Right  ("foo", "bar"), "")
    it "should parse nested xml" $ runParser xmlElementP "<tag foo=\"eccomi\"></tag>"  `shouldBe` (Right  (XMLElement "tag" [("foo", "eccomi")] []), "")






