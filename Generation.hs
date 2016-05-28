module Generation where
import Stangry
import Kills
import Moves

import Control.Monad
import Data.List
import Data.Char
import Debug.Trace

genGeneration pla = genGenerationsFromArr [pla] 1000
genGenerationsN pla = genGenerationsFromArr [pla]

genGenerationsFromArr::[Plansza]->Int->Kolor->[Plansza]
genGenerationsFromArr _ 0 _ = []
genGenerationsFromArr [] _ _ = []
genGenerationsFromArr plas cnt kol = do
     let kills = genGenerKills plas kol
     let moves = genGenerMoves plas kol
     if not (null kills) then
        kills ++ genGenerationsFromArr kills (cnt - 1) (inverse kol) 
     else
        moves ++ genGenerationsFromArr moves (cnt - 1) (inverse kol)
         
genGenerKills [] _ = []
genGenerKills plas kol = concat $ concatMap (`getKillBest` kol) plas

genGenerMoves [] _ = []
genGenerMoves plas kol = concatMap (`getMovesAllBrds` kol) plas

ifWin::Plansza->Kolor->Bool
ifWin pla kol = count pla (inverse kol) == 0 || null (genGenerationsN pla 1 (inverse kol))