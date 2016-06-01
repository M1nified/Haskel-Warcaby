module StangryTest where
    
import Test.Hspec
    
import Stangry

test = do
    describe "get303" $ do
        it "should get 3rd elem of tuple" $ do
            get3o3(1,2,3) `shouldBe` 3