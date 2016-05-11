import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad

import Stangry

main = do
    pokaz pl
    print pl
    
initB = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
pl = Plansza (parseGame initB) 12 12
init2 = ".b.b.b.b\n........\n.b.b.b..\n........\n.....b..\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
pl2 = Plansza (parseGame init2) 12 12


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
    let hp = Pole (9-x) (9-y) (typ h) (kolor h)
    let tp = runFindPositionsInRow t y
    return hp++tp

--poruszanie 

--szukanie mozliwych ruchow
--moves f board = (genKill f b, genMoves f b)


-- MOVE

-- WYCIAGANIE SASIADOW
    
getElem UL = getElemUL
getElem UR = getElemUR
getElem DL = getElemDR
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

getMove UL = getMoveUL
getMove UR = getMoveUR
getMove DL = getMoveDL
getMove DR = getMoveDR

getMoveUL pla x y = do
    let pl = plansza pla
    let el = getElemAt pl x y
    let nell = getElemUL pla x y
    when (null nell) []
    let nel = head nell
    if  typ nel == Wolne then
        if typ nel == Damka then
        nel : getElemUL pla (x - 1) (y - 1)
        else
        [nel]
    else []
getMoveUR pla x y = do
    let pl = plansza pla
    let el = getElemAt pl x y
    let nell = getElemUR pla x y
    when (null nell) []
    let nel = head nell
    if typ nel == Wolne then
        if typ nel == Damka then
            nel : getElemUR pla (x+1) (y-1)
        else
            [nel]
    else []
getMoveDL pla x y = do
    let pl = plansza pla
    let el = getElemAt pl x y
    let nell = getElemDL pla x y
    when (null nell) []
    let nel = head nell
    if typ nel == Wolne then
        if typ nel == Damka then
            nel : getMoveDL pla (x-1) (y+1)
        else
            [nel]
    else []
getMoveDR pla x y = do
    let pl = plansza pla
    let el = getElemAt pl x y
    let nell = getElemDR pla x y
    when (null nell) []
    let nel = head nell
    if typ nel == Wolne then
        if typ nel == Damka then
            nel : getElemDR pla (x+1) (y+1)
        else
            [nel]
    else []

genMoves Pole{x=x,y=y,typ=Pionek,kolor=Biale} pl = getElemUL pl x y ++ getElemUR pl x y
genMoves Pole{x=x,y=y,typ=Pionek,kolor=Czarne} pl = getMoveDL pl x y ++ getElemDR pl x y
genMoves Pole{x=x,y=y,typ=Damka} pl = getElemUL pl x y ++ getElemUR pl x y ++ getMoveDL pl x y ++ getElemDR pl x y

replaceRow y newrow pl = take (y-1) pl ++ newrow : drop y pl 
replace x y val pla = do
    let row = plansza pla !! (y - 1)
    let nr = take (x-1) row ++ val : drop x row
    replaceRow y nr (plansza pla)
    
    
-- move from x y to destination
move::Plansza -> Int -> Int -> Int -> Int -> Plansza
move pla x y dx dy = do
    let pl = plansza pla
    let el = getElemAt pl x y
    let put = Plansza (replace dx dy el pla) (numB pla) (numW pla)
    Plansza (replace x y (Pole x y Wolne Brak) put) (numB pla) (numW pla)
    
-- KILLS
getKillsP pla x y = getKills DL pla x y
-- ++ getKills DL pla x y ++ getKills DR pla x y

getKillsHelp pla targ land = do
    when (null land) []
    -- [(t,land)]
    -- [t]
    let nex = uncurry (getKillsP pla) (poleXY (head land))
    -- [(1,[1]),(1,[2])]
    if null nex then
        [(targ, [head land])]
    else
        [(fst (head nex), land ++ snd (head nex))]
        
getKills::Direction -> Plansza -> Int -> Int -> [(Pole,[Pole])]
getKills dir pla x y = do
    let me = getElemAt2 pla x y
    let targ = getElem dir pla x y
    when (null targ || (typ (head targ) == Wolne) || (kolor me == kolor (head targ))) []
    let t = head targ
    let xy = poleXY t
    let land = uncurry (getMove dir pla) xy -- move nic nie zwroci jesli nie bedzie miejsca do ladowania
    getKillsHelp pla t land    