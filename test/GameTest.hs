module GameTest where
    
import Test.Hspec
    
import Stangry
import Game


test = do
    describe "parseElem" $ do
        it "parse letter as Pole" $ do
            parseElem "b" `shouldBe` Pole 0 0 Pionek Czarne