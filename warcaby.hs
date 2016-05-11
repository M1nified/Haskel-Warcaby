import Data.List.Split
import Data.List

main = do
    let dane = parseGame initB
    pokaz dane
    print "Hello"
    
initB = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."


everyNth lst n = [snd x | x <- zip [1 ..] lst, fst x `mod` n == 0]

parseElem::String -> Pole
parseElem "b" = Pole 0 0 Pionek Czarne
parseElem "w" = Pole 0 0 Pionek Biale
parseElem _ = Pole 0 0 Wolne Brak

blowLine line = filter (/= "") (splitOn ""  line)--(\x -> x /= "")

parseLine line = do
    let l = blowLine line
    map parseElem l 
    
parseGame l = runFindPositions(map parseLine (splitOn "\n" l))

--ustalanie pozycji
runFindPositions mapa = findPositions mapa (length mapa)
findPositions::[[Pole]] -> Int -> [[Pole]]
findPositions [[]] _ = [[]]
findPositions [] _ = [[]]
findPositions mapa y = do
    let t = tail mapa
    let tp = runFindPositions (t)
    let h = runFindPositionsInRow (head mapa) y
    let tpp = if length tp > 0
        then tp
        else []
    return h++tpp
    
runFindPositionsInRow row y = findPositionsInRow row (length row) y
findPositionsInRow :: [Pole] -> Int -> Int -> [Pole]
findPositionsInRow [] _ _ = []
findPositionsInRow row x y = do
    let t = tail row
    let h = head row
    let hp = Pole x y (typ h) (kolor h)
    let tp = runFindPositionsInRow t y
    return hp++tp
    
    
--pokaz plansze
pokaz = mapM print

data Kolor = Biale | Czarne | Brak
    deriving (Show)
data TypNaDanymPolu = Pionek | Krolowa | Wolne
    deriving (Show)
data Pole = Pole { x :: Int, y :: Int, typ :: TypNaDanymPolu, kolor :: Kolor }
  -- deriving (Show)
instance Show Pole where
    show Pole{kolor = Biale, typ = Pionek} = show 'w'
    show Pole{kolor = Biale, typ = Krolowa} = show "W"
    show Pole{kolor = Czarne, typ = Pionek} = show "b"
    show Pole{kolor = Czarne, typ = Krolowa} = show "B"
    show Pole{} = show " "
    
--poruszanie 