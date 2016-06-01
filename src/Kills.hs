module Kills where
import Stangry
import Moves
import Control.Monad
import Data.List
-- KILLS
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
    let lxy = poleXY (fst (head land))
    let npla1 = uncurry (remove pla) xy
    let npla = swapQueens $ uncurry (move npla1 x y ) lxy
    let nex = uncurry (genKillsP npla) lxy
    if null nex then
        [npla]
    else
        nex

genDistanceKill::Direction->Plansza->Int->Int->[Plansza]
genDistanceKill dir pla x y = do
    let me = getElemAt2 pla x y
    let next = getElem dir pla x y
    if not (null next) && typ (head next) == Wolne then
        uncurry (genDistanceKill dir (moveF2F pla me (head next))) (poleXY (head next))
    else
        genKills dir pla x y
    
genKillsP pla x y = genKills UL pla x y ++ genKills UR pla x y ++ genKills DL pla x y ++ genKills DR pla x y
genKillsQueenP pla x y = genDistanceKill UL pla x y ++ genDistanceKill UR pla x y ++ genDistanceKill DL pla x y ++ genDistanceKill DR pla x y

genKillsForPole pla Pole{x=x,y=y,typ=Damka} = genKillsQueenP pla x y
genKillsForPole pla Pole{x=x,y=y} = genKillsP pla x y
    

getKillsAll pla kol = concatMap (genKillsForPole pla) $ findByColor pla kol
    
getKillBest::Plansza->Kolor->[[Plansza]]
getKillBest pla kol = [findBestKill (getKillsAll pla kol) kol]
getKillBestSingle::Plansza->Kolor->[Plansza]
getKillBestSingle pla kol = concat $ getKillBest pla kol
    
findBestKill [] _ = []
findBestKill kills kolor = [minimumBy (\a b -> cmpPlanszaForKolor a b kolor) kills]
cmpPlanszaForKolor pla1 pla2 Biale = compare (numB pla1) (numB pla2)
cmpPlanszaForKolor pla1 pla2 Czarne = compare (numW pla1) (numW pla2)