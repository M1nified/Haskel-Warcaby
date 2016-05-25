import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad

import Stangry

main = do
    let kk = head (genKillsP pl2 7 6)
    let k = genKills DL kk 3 2
    pokaz kk
    print k
    
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

getMoves Pole{x=x,y=y,typ=Pionek,kolor=Biale} pla = getMoveUL pla x y ++ getMoveUR pla x y
getMoves Pole{x=x,y=y,typ=Pionek,kolor=Czarne} pla = getMoveDL pla x y ++ getMoveDR pla x y
getMoves Pole{x=x,y=y,typ=Damka} pla = getMoveUL pla x y ++ getMoveUR pla x y ++ getMoveDL pla x y ++ getMoveDR pla x y

getMovesP pla x y = do
    let el = getElemAt2 pla x y
    --el
    getMoves el pla
    
-- KILLS
genKillsP pla x y = genKills UL pla x y ++ genKills UR pla x y ++ genKills DL pla x y ++ genKills DR pla x y

genKills::Direction -> Plansza -> Int -> Int -> [Plansza]
genKills dir pla x y = do
    let me = getElemAt2 pla x y
    let targ = getElem dir pla x y
    when (null targ
        || typ me == Wolne 
        || (typ (head targ) == Wolne) 
        || (kolor me == kolor (head targ))
        ) []
    let t = head targ
    let xy = poleXY t
    let land = uncurry (getMove dir pla) xy -- move nic nie zwroci jesli nie bedzie miejsca do ladowania
    when (null land) []
    let lxy = poleXY (head land)
    -- [(t,land)]
    -- [t]
    let npla1 = uncurry (remove pla) xy
    let npla = uncurry (move npla1 x y ) lxy
    let nex = uncurry (genKillsP npla) lxy
    -- [(1,[1]),(1,[2])]
    if null nex then
        [npla]
    else
        nex
    -- if null nex then
    --     [(t, [head land])]
    -- else
    --     [(fst (head nex), land ++ snd (head nex))]


k = head (genKillsP pl2 7 6)
k2 = last (genKillsP pl2 7 6)

--genKillsAll::Plansza -> Kolor -> [Plansza]
-- genKillsAll pla kol = do
--     let pl = plansza pla
--     mapM (mapM (genKillsAllEach pla kol)) pl

-- genKillsAllEach::Plansza->Kolor->Pole->Plansza
-- genKillsAllEach pla kol pol = if kolor pol == kol then uncurry (genKillsP pla) (poleXY pol) else []

