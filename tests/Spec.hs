import Test.Hspec

import Expression
import Reduction
import Parsec
import Parse

main :: IO ()
main = do
  testFunctionParsing
  testApplicationParsing
  testNameParsing
  testExpressionParsing

  testFunctionReduction
  testApplicationReduction
  testNameReduction
  testExpressionReduction

-- Parsing Tests

testFunctionParsing :: IO ()
testFunctionParsing = hspec $ do
  describe "Parsing a function" $ do
    it "returns Func x (Name x) when parsed \\x.x" $
      (regularParse function "\\x.x") `shouldBe` Right (Func "x" (Name "x"))
  
    it "returns Func x (Func y (Name x)) when parsed \\x.\\y.x" $
      (regularParse function "\\x.\\y.x y") `shouldBe` Right (Func "x" (Func "y" (Name "x")))

testApplicationParsing :: IO ()
testApplicationParsing = hspec $ do
  describe "Parsing an application" $ do
    it "returns FuncApp (Func x (Name x) (Name y)) when parsed (\\x.x y)" $
      (regularParse application "(\\x.x y)") `shouldBe` Right (FuncApp (Func "x" (Name "x")) (Name "y"))
    it "returns FuncApp (Func x (Name x)) (Func y (Name y)) when parsed (\\x.x \\y.y)" $
      (regularParse application "(\\x.x \\y.y)") `shouldBe` Right (FuncApp (Func "x" (Name "x")) (Func "y" (Name "y")))

testNameParsing :: IO ()
testNameParsing = hspec $ do
  describe "Parsing a name" $ do
    it "returns Name x when parsed x" $
      (regularParse name "x") `shouldBe` Right (Name "x")
    it "returns Name x123 when parsed x123" $
      (regularParse name "x123") `shouldBe` Right (Name "x123")
    it "returns Name dog when parsed dog" $
      (regularParse name "dog") `shouldBe` Right (Name "dog")

testExpressionParsing :: IO ()
testExpressionParsing = hspec $ do
  describe "Parsing an expression" $ do
    it "returns Func x (Name x) when parsed \\x.x" $
      (regularParse expression "\\x.x") `shouldBe` Right (Func "x" (Name "x"))
    it "returns Func x (Func y (Name x)) when parsed \\x.\\y.x" $ do
      (regularParse expression "\\x.\\y.x") `shouldBe` Right (Func "x" (Func "y" (Name "x")))
    it "returns Func x (FuncApp (Name x) (Name x)) when parsed \\x.(x x)" $ do
      (regularParse expression "\\x.(x x)") `shouldBe` Right (Func "x" (FuncApp (Name "x") (Name "x")))

-- Reduction Tests

testFunctionReduction :: IO ()
testFunctionReduction = hspec $ do
  describe "Reducing a function" $ do
    it "returns Func x after reduction of \\x.x" $
      (reduce <$> regularParse function "\\x.x") `shouldBe` Right (Func "x" (Name "x"))

testApplicationReduction :: IO ()
testApplicationReduction = hspec $ do
  describe "Reducing a function" $ do
    it "returns Name x after reduction of (\\y.y x)" $
      (reduce <$> regularParse application "(\\y.y x)") `shouldBe` Right (Name "x")
    it "returns Func x Name x after reduction of (\\y.y \\x.x)" $
      (reduce <$> regularParse application "(\\y.y \\x.x)") `shouldBe` Right (Func "x" (Name "x")) 

testNameReduction :: IO ()
testNameReduction = hspec $ do
  describe "Reducing a name" $ do
    it "returns Name x" $
      (reduce <$> regularParse name "x") `shouldBe` Right (Name "x")
      

testExpressionReduction :: IO ()
testExpressionReduction = hspec $ do
  describe "Reducing an expression" $ do
    
