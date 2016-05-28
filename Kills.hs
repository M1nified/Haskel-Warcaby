module Kills where
import Stangry
import Moves
import Control.Monad
import Data.List
-- KILLS
genKillsP pla x y = genKills UL pla x y ++ genKills UR pla x y ++ genKills DL pla x y ++ genKills DR pla x y

genKillsForPole pla pol = uncurry (genKillsP pla) (poleXY pol)

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


--genKillsAll::Plansza -> Kolor -> [Plansza]
-- genKillsAll pla kol = do
--     let pl = plansza pla
--     mapM (mapM (genKillsAllEach pla kol)) pl

-- genKillsAllEach::Plansza->Kolor->Pole->Plansza
-- genKillsAllEach pla kol pol = if kolor pol == kol then uncurry (genKillsP pla) (poleXY pol) else []

getKillsAll pla kol = do
    let pola = findByColor pla kol
    concatMap (genKillsForPole pla) pola
    
getKillBest pla kol = do
    let allKills = getKillsAll pla kol
    findBestKill allKills kol
    
findBestKill kills kolor = minimumBy (\a b -> cmpPlanszaForKolor a b kolor) kills
cmpPlanszaForKolor pla1 pla2 Biale = compare (numB pla1) (numB pla2)
cmpPlanszaForKolor pla1 pla2 Czarne = compare (numW pla1) (numW pla2)