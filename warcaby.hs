import Data.List.Split
import Data.List

main = do
    let da = parse initB
    pokaz da
    print("Hello")
    
initB = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."


everyNth lst n = [snd x | x <- (zip [1..] lst), fst x `mod` n == 0]
parseElem::String -> Pole
parseElem "b" = Pole 0 0 Pionek Czarne
parseElem "w" = Pole 0 0 Pionek Biale
parseElem _ = Pole 0 0 Wolne Brak
blowLine line = filter (\x -> x /= "") (splitOn ""  line)
parseLine line = do
    let l = blowLine line
    map parseElem l 
parse l = do
    map parseLine (splitOn "\n" l)
    
--pokaz plansze
pokaz plansza = mapM print plansza

data Kolor = Biale | Czarne | Brak
    deriving (Show)
data TypNaDanymPolu = Pionek | Krolowa | Wolne
    deriving (Show)
data Pole = Pole {
    x :: Int, y :: Int, typ :: TypNaDanymPolu, kolor :: Kolor
    }
instance Show Pole where
    show(Pole {kolor=Biale,typ=Pionek}) = show 'w'
    show(Pole {kolor=Biale,typ=Krolowa}) = show "W"
    show(Pole {kolor=Czarne,typ=Pionek}) = show "b"
    show(Pole {kolor=Czarne,typ=Krolowa}) = show "B"
    show(Pole {}) = show " "