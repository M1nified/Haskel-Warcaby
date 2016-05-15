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
    } --deriving (Show)
instance Show Pole where
    show Pole{x=x,y=y,kolor = Biale, typ = Pionek} = show "w" ++ [intToDigit x] ++ [intToDigit y]
    show Pole{x=x,y=y,kolor = Biale, typ = Damka} = show "W" ++ [intToDigit x] ++ [intToDigit y]
    show Pole{x=x,y=y,kolor = Czarne, typ = Pionek} = show "b" ++ [intToDigit x] ++ [intToDigit y]
    show Pole{x=x,y=y,kolor = Czarne, typ = Damka} = show "B" ++ [intToDigit x] ++ [intToDigit y]
    show Pole{x=x,y=y} = show " " ++ [intToDigit x] ++ [intToDigit y]
    
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
    mapM_ pokazWiersz (zip [1 ..] plan)
    print (' ' : ' ' : map intToDigit (take 8 [1 ..]))
    

-- ZMIENIANIE
replaceRow y newrow pl = take (y-1) pl ++ newrow : drop y pl 

replace x y val pla = do
    let row = plansza pla !! (y - 1)
    let nr = take (x-1) row ++ val : drop x row
    replaceRow y nr (plansza pla)

remove pla x y = do
    let el = getElemAt2 pla x y
    --dopisac odejmowanie z licznika
    let pl = replace x y (Pole x y Wolne Brak) pla
    let nums = decNum pla el
    uncurry (Plansza pl) nums

decNum pla Pole{kolor = Biale} = (numB pla, numW pla - 1)
decNum pla Pole{kolor = Czarne} = (numB pla - 1, numW pla)
    
-- move from x y to destination
move::Plansza -> Int -> Int -> Int -> Int -> Plansza
move pla x y dx dy = do
    let pl = plansza pla
    let el = getElemAt pl x y
    let nel = Pole dx dy (typ el) (kolor el)
    let put = Plansza (replace dx dy nel pla) (numB pla) (numW pla)
    Plansza (replace x y (Pole x y Wolne Brak) put) (numB pla) (numW pla)

-- WYBIERANIE
getElemAt pl x y = pl !! (y-1) !! (x-1) 
getElemAt2 pla = getElemAt (plansza pla)
    
-- isFree Pole{typ=t} = t == Wolne