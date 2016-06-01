module Generation where
import Stangry
import Kills
import Moves

import Control.Monad
import Data.List
import Data.Char
import Data.Function
import Debug.Trace

genGeneration pla = genGenerationsFromArrRun [pla] 1000
genGenerationsN pla = genGenerationsFromArrRun [pla]

genGenerationsFromArrRun::[Plansza]->Int->Kolor->[MoveResult]
genGenerationsFromArrRun plas cnt kol = do
    let kills = genGenerKills plas kol
    let moves = genGenerMoves plas kol
    let tmp = if not (null kills) then
            map (\x -> (x,genGenerationsFromArr [x] (cnt - 1) kol)) kills
        else
            map (\x -> (x,genGenerationsFromArr [x] (cnt - 1) kol)) moves
    concatMap toMR tmp
    -- tmp
toMR::(Plansza,[SearchResult])->[MoveResult]
toMR (pla,sr) = do
    if not $ null sr then
        [(pla, fst $ head sr, snd $ head sr)]
    else
        [(pla,-1,[])]
        
genGenerationsFromArr::[Plansza]->Int->Kolor->[SearchResult]
genGenerationsFromArr _ 0 _ = []
genGenerationsFromArr [] _ _ = []
genGenerationsFromArr plas cnt kol = do
    --  when (ifAnyWins plas kol || ifAnyWins plas (inverse kol)) []
     let kills = genGenerKills plas kol
    --  let kills = []
     let moves = genGenerMoves plas kol
     if not (null kills) then
         if cnt == 1 || ifAnyWins kills kol then
             [(cnt,kills)]
         else
             genGenerationsFromArr kills (cnt - 1) (inverse kol) 
     else
         if cnt == 1 || ifAnyWins moves kol || isTooMuch moves then
             [(cnt,moves)]
         else
             genGenerationsFromArr moves (cnt - 1) (inverse kol)
      
genGenerKills::[Plansza]->Kolor->[Plansza]   
genGenerKills [] _ = []
genGenerKills plas kol = concat $ concatMap (`getKillBest` kol) plas

genGenerMoves::[Plansza]->Kolor->[Plansza]   
genGenerMoves [] _ = []
genGenerMoves plas kol = concatMap (`getMovesAllBrds` kol) plas

ifWins::Plansza->Kolor->Bool
ifWins pla kol = count pla (inverse kol) == 0 || null (genGenerationsN pla 1 (inverse kol))

ifAnyWins::[Plansza]->Kolor->Bool
ifAnyWins [] _ = False
ifAnyWins plas kol = True `elem` map (`ifWins` kol) plas

findWinner::[MoveResult]->Kolor->[MoveResult]
findWinner mrs kol = findWinner3 ( filter (`findWinner2` kol) mrs)
findWinner2::MoveResult->Kolor->Bool
findWinner2 (pla,-1,_) kol = ifWins pla kol
findWinner2 mr kol = ifAnyWins (get3o3 mr) kol
findWinner3::[MoveResult]->[MoveResult]
findWinner3 [] = []
findWinner3 mrs = [maximumBy (compare `on` get2o3) mrs] 
--(\a b -> compare (get2o3 a) (get2o3 b))
        
findBestDecision::[MoveResult]->Kolor->[MoveResult]
findBestDecision [] _ = []
findBestDecision mrs kol = [minimumBy (\a b -> cmpMoveResults a b kol) mrs]
cmpMoveResults mr1 mr2 kol = do
    let r1 = findBestKill (get3o3 mr1) kol
    let r2 = findBestKill (get3o3 mr2) kol
    compare 1 2
    -- cmpPlanszaForKolor (head r1) (head r2) kol
    
getBestMR::Plansza->Kolor->[MoveResult]
getBestMR pla kol = do
    let mrs = genGenerationsN pla 1000 kol
    let win = findWinner mrs kol
    let step = findBestDecision mrs kol
    if not (null win) then
        win
    else
        step
    
getTheMovePP::Plansza -> Kolor -> [MoveP]
getTheMovePP pla kol = do
    let mrs = getBestMR pla kol
    [(pla, get1o3 $ head mrs) | not (null mrs)]
    
getTheMoveP::Plansza->Kolor->[Plansza]
getTheMoveP pla kol = do
    let ms = getTheMovePP pla kol
    [snd (head ms) | not (null ms)]
    
getDepth::Plansza->Kolor->Int
getDepth pla kol = do
    let num = 50 - (count pla Biale + count pla Czarne)
    num * 4

isTooMuch::[Plansza]->Bool
isTooMuch pla = length pla > 100000