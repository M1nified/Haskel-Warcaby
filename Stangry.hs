module Stangry where
import Data.Char
    
data Direction = UL | UR | DL | DR
    deriving (Show, Eq)
    
data TypNaDanymPolu = Pionek | Damka | Wolne
    deriving (Show, Eq)
data Kolor = Biale | Czarne | Brak
    deriving (Show, Eq)
data Pole = Pole {
    x :: Int, 
    y :: Int,
    typ :: TypNaDanymPolu,
    kolor :: Kolor
    } deriving (Show)
-- instance Show Pole where
--     show Pole{kolor = Biale, typ = Pionek} = show 'w'
--     show Pole{kolor = Biale, typ = Damka} = show "W"
--     show Pole{kolor = Czarne, typ = Pionek} = show "b"
--     show Pole{kolor = Czarne, typ = Damka} = show "B"
--     show Pole{} = show " "
    
--plansza to [[Pole]]
data Plansza = Plansza {
    plansza :: [[Pole]],
    numB :: Int,
    numW :: Int
    } deriving (Show)
    
--pokaz plansze
tlumaczPole::Pole -> Char
tlumaczPole Pole{kolor = Biale, typ = Pionek} = 'w'
tlumaczPole Pole{kolor = Biale, typ = Damka} = 'W'
tlumaczPole Pole{kolor = Czarne, typ = Pionek} = 'b'
tlumaczPole Pole{kolor = Czarne, typ = Damka} = 'B'
tlumaczPole Pole{} = ' '

poleXY Pole{x=x,y=y} = (x,y)

pokazWiersz (num, row) = do
    let str = ' ' : map tlumaczPole row
    let n = intToDigit num
    let line = n : str
    print line
pokaz pl = do 
    let plan = take 8 (plansza pl)
    print (' ' : ' ' : map intToDigit (take 8 [1 ..]))
    mapM pokazWiersz (zip [1..] plan)

-- WYBIERANIE
getElemAt pl x y = pl !! (y-1) !! (x-1) 
getElemAt2 pl = getElemAt (plansza pl)
    
-- isFree Pole{typ=t} = t == Wolne