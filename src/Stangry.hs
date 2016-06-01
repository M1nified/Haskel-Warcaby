module Stangry where
import Control.Monad
import Data.Char

type MoveResult = (Plansza,Int,[Plansza]) 
type SearchResult = (Int,[Plansza])
type MoveP = (Plansza,Plansza)

get1o3::(a,b,c)->a
get1o3 (x,_,_) = x
get2o3::(a,b,c)->b
get2o3 (_,x,_) = x
get3o3::(a,b,c)->c
get3o3 (_,_,x) = x

data Direction = UL | UR | DL | DR
    deriving (Show, Eq)
    
data TypNaDanymPolu = Pionek | Damka | Wolne
    deriving (Show, Eq)
data Kolor = Biale | Czarne | Brak
    deriving (Show, Eq)
inverse::Kolor->Kolor
inverse Biale = Czarne
inverse Czarne = Biale
data Pole = Pole {
    x :: Int, 
    y :: Int,
    typ :: TypNaDanymPolu,
    kolor :: Kolor
    } deriving (Eq)
instance Show Pole where
    show Pole{x=x,y=y,kolor = Biale, typ = Pionek} = show "w" ++ [intToDigit x] ++ [intToDigit y]
    show Pole{x=x,y=y,kolor = Biale, typ = Damka} = show "W" ++ [intToDigit x] ++ [intToDigit y]
    show Pole{x=x,y=y,kolor = Czarne, typ = Pionek} = show "b" ++ [intToDigit x] ++ [intToDigit y]
    show Pole{x=x,y=y,kolor = Czarne, typ = Damka} = show "B" ++ [intToDigit x] ++ [intToDigit y]
    show Pole{x=x,y=y} = show " " ++ [intToDigit x] ++ [intToDigit y]
    
count::Plansza->Kolor->Int
count pla Biale = numW pla
count pla Czarne = numB pla
--plansza to [[Pole]]
data Plansza = Plansza {
    plansza :: [[Pole]],
    numB :: Int,
    numW :: Int
    } deriving (Eq)
instance Show Plansza where
    show = toStr
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
pokaz pla = do 
    let plan = take 8 (plansza pla)
    print (' ' : ' ' : map intToDigit (take 8 [1 ..]))
    mapM_ pokazWiersz (zip [1 ..] plan)
    print (' ' : ' ' : map intToDigit (take 8 [1 ..]))
    
toStrRow (num,row) = do
    let str = ' ' : map tlumaczPole row
    let n = intToDigit num
    n : str ++ "\n"
toStr pla = do
    let plan = take 8 (plansza pla)
    (' ' : ' ' : map intToDigit (take 8 [1 ..])) ++ "\n" ++ concatMap toStrRow (zip [1 ..] plan) ++ (' ' : ' ' : map intToDigit (take 8 [1 ..])) ++ "\n"
    

-- ZMIENIANIE
replaceRow y newrow pl = take (y-1) pl ++ newrow : drop y pl 

replace x y val pla = do
    let row = plansza pla !! (y - 1)
    let nr = take (x-1) row ++ val : drop x row
    replaceRow y nr (plansza pla)

-- remove Plansza{plansza=[],numB=b,numW=w} _ _ = Plansza [] b w
remove pla x y = do
    let el = getElemAt2 pla x y
    --dopisac odejmowanie z licznika
    let pl = replace x y (Pole x y Wolne Brak) pla
    -- when (null pl) pla
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
moveF2F::Plansza->Pole->Pole->Plansza
moveF2F pla Pole{x=x1,y=y1} Pole{x=x2,y=y2} = move pla x1 y1 x2 y2

-- WYBIERANIE
getElemAt pl x y = pl !! (y-1) !! (x-1) 
getElemAt2 pla = getElemAt (plansza pla)
    
-- isFree Pole{typ=t} = t == Wolne

--ZNAJDOWANIE POL
findByColor::Plansza->Kolor->[Pole]
findByColor pla = findByColorRun (plansza pla)

findByColorRun::[[Pole]]->Kolor->[Pole]
findByColorRun [[]] _ = []
findByColorRun [] _ = []
findByColorRun pls kol = concatMap (filter $ \x -> kolor x == kol ) pls


-- WYCIAGANIE SASIADOW
    
getElem UL = getElemUL
getElem UR = getElemUR
getElem DL = getElemDL
getElem DR = getElemDR
--up left
getElemUL _ 1 _ = []
getElemUL _ _ 1 = []
getElemUL pla x y = [getElemAt2 pla (x-1) (y-1)]
  
--up right
getElemUR _ 8 _ = []
getElemUR _ _ 1 = []
getElemUR pla x y = [getElemAt2 pla (x+1) (y-1)]
    
--down left
getElemDL _ _ 8 = []
getElemDL _ 1 _ = []
getElemDL pla x y = [getElemAt2 pla (x - 1) (y + 1)]

--down right
getElemDR _ _ 8 = []
getElemDR _ 8 _ = []
getElemDR pla x y = [getElemAt2 pla (x+1) (y+1)]

swapQueens pla = do
    let pl = map (map swapQueen) (plansza pla)
    Plansza pl (numB pla) (numW pla)
    
swapQueen Pole{x=x,y=1,kolor=Biale,typ=Pionek} = Pole x 1 Damka Biale 
swapQueen Pole{x=x,y=8,kolor=Czarne,typ=Pionek} = Pole x 8 Damka Czarne 
swapQueen pol = pol