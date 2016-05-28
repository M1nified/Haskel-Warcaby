import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad

import Stangry
import Moves
import Kills
import Generation

main = do
    let kk = head (genKillsP pl2 7 6)
    let k = genKills DL kk 3 2
    pokaz kk
    print k
    
initB = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
pl = Plansza (parseGame initB) 12 12
init2 = ".b.b.b.b\n........\n.b.b.b..\n........\n.....b..\nW.w.w.w.\n...w.w.w\nw.w.w.w."
pl2 = Plansza (parseGame init2) 12 12

k = head (genKillsP pl2 7 6)
k2 = last (genKillsP pl2 7 6)

everyNth lst n = [snd x | x <- zip [1 ..] lst, fst x `mod` n == 0]

parseElem::String -> Pole
parseElem "b" = Pole 0 0 Pionek Czarne
parseElem "w" = Pole 0 0 Pionek Biale
parseElem "B" = Pole 0 0 Damka Czarne
parseElem "W" = Pole 0 0 Damka Biale
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