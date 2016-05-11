import Data.List.Split
import Data.List

main = do
    pokazPlansze pl
    print pl
    
initB = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
pl = Plansza (parseGame initB) 12 12


everyNth lst n = [snd x | x <- zip [1 ..] lst, fst x `mod` n == 0]

parseElem::String -> Pole
parseElem "b" = Pole 0 0 Pionek Czarne
parseElem "w" = Pole 0 0 Pionek Biale
parseElem "B" = Pole 0 0 Damka Biale
parseElem "W" = Pole 0 0 Damka Czarne
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
    let tp = runFindPositions t
    let h = runFindPositionsInRow (head mapa) y
    let tpp = if not (null tp)
        then tp
        else []
    return h++tpp
    
runFindPositionsInRow row = findPositionsInRow row (length row)
findPositionsInRow :: [Pole] -> Int -> Int -> [Pole]
findPositionsInRow [] _ _ = []
findPositionsInRow row x y = do
    let t = tail row
    let h = head row
    let hp = Pole (8-x) (8-y) (typ h) (kolor h)
    let tp = runFindPositionsInRow t y
    return hp++tp

data Kolor = Biale | Czarne | Brak
    deriving (Show)
data TypNaDanymPolu = Pionek | Damka | Wolne
    deriving (Show)
data Pole = Pole { x :: Int, y :: Int, typ :: TypNaDanymPolu, kolor :: Kolor }
--   deriving (Show)
instance Show Pole where
    show Pole{kolor = Biale, typ = Pionek} = show 'w'
    show Pole{kolor = Biale, typ = Damka} = show "W"
    show Pole{kolor = Czarne, typ = Pionek} = show "b"
    show Pole{kolor = Czarne, typ = Damka} = show "B"
    show Pole{} = show " "
    
--plansza to [[Pole]]
data Plansza = Plansza {
    plansza :: [[Pole]],
    numB :: Int,
    numW :: Int
    } deriving (Show)
--pokaz plansze
pokazPlansze pl= mapM print (plansza pl)

--poruszanie 

--szukanie mozliwych ruchow
--moves f board = (genKill f b, genMoves f b)
-- genMoves::Pole -> Plansza -> Int
getElemAt pl x y = pl !! (y-1) !! (x-1) 
getElemAt2 pl = getElemAt (plansza pl)
--up left
getElemUL Plansza{plansza=pl} x y = getElemAt pl (x-1) (y-1)
--up right
getElemUR Plansza{plansza=pl} x y = getElemAt pl (x+1) (y-1)
--down left
getElemDL Plansza{plansza=pl} x y = getElemAt pl (x-1) (y+1)
--down right
getElemDR Plansza{plansza=pl} x y = getElemAt pl (x+1) (y+1)

genMoves Pole{x=x,y=y,typ=Pionek,kolor=kolor} pl =
    let r = [] ++ getElemUL pl x y ++ getElemUR pl x y
    filter () r