module InterpreterSpec (main, spec) where


import Test.Hspec
import Query

specBetween :: Spec
specBetween = do
  describe "phrasesBetween" $ do
    it "everything between two words" $
      phraseBetween "is" "to" (query "This is what we have to do") `shouldBe` Just "what we have"
    it "empty if one second word does not exist" $
      phraseBetween "is" "nothing" (query "This is what we have to do") `shouldBe` Nothing
    it "empty if one first word does not exist" $
      phraseBetween "nothing" "to" (query "This is what we have to do") `shouldBe` Nothing

specAfter :: Spec
specAfter = do
  describe "phraseAfter" $ do
    it "words after first argument" $
      phraseAfter "is" (query "This is what we have to do") `shouldBe` Just "what we have to do"
    it "epmty if the argument is not present" $
      phraseAfter "nothing" (query "This is what we have to do") `shouldBe` Nothing


spec :: Spec
spec = do
  specBetween
  specAfter

main :: IO ()
main = hspec spec
