module Generation where
import Stangry
import Kills
import Moves

import Control.Monad
import Data.List

genGeneration pla = genGenerationsFromArr [pla] 2

genGenerationsFromArr _ 0 _ = []
genGenerationsFromArr plas cnt kol = do
     let kills = genGenerKills plas kol
     let moves = genGenerMoves plas kol 
    --  if not (null kills) then
    --      kills ++ genGenerationsFromArr kills (cnt - 1) (inverse kol) 
    --  else
     moves ++ genGenerationsFromArr moves (cnt - 1) (inverse kol)
         
genGenerKills plas kol = map (`getKillBest` kol) plas

genGenerMoves plas kol = concatMap (`getMovesAllBrds` kol) plas