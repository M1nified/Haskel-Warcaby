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
pl = parseGame initB
init2 = ".b.b.b.b\n........\n.b.b.b..\n........\n.....b..\nW.w.w.w.\n...w.w.w\nw.w.w.w."
pl2 = parseGame init2
init3 = "........\n........\n........\n........\n.....b..\nW.w.w.w.\n...w.w.w\nw.w.w.w."
pl3 = parseGame init3

k = head (genKillsP pl2 7 6)
k2 = last (genKillsP pl2 7 6)

everyNth lst n = [snd x | x <- zip [1 ..] lst, fst x `mod` n == 0]

parseElem::String -> Pole
parseElem "b" = Pole 0 0 Pionek Czarne
parseElem "w" = Pole 0 0 Pionek Biale
parseElem "B" = Pole 0 0 Damka Czarne
parseElem "W" = Pole 0 0 Damka Biale
parseElem _ = Pole 0 0 Wolne Brak

parseElemXY::Char->Int->Int -> Pole
parseElemXY 'b' x y = Pole x y Pionek Czarne
parseElemXY 'w' x y = Pole x y Pionek Biale
parseElemXY 'B' x y = Pole x y Damka Czarne
parseElemXY 'W' x y = Pole x y Damka Biale
parseElemXY _ x y = Pole x y Wolne Brak

blowLine line = filter (/= "") (splitOn ""  line)--(\x -> x /= "")

parseGame l = do
    let rows = zip [1..] $ map (zip [1..]) $ splitOn "\n" l
    let pl = map (\y -> map (\x -> parseElemXY (snd x) (fst x) (fst y) ) $ snd y) rows
    Plansza pl (length $ findByColorRun pl Czarne) (length $ findByColorRun pl Biale)
    
    
gameStart pla = play pla Biale
    
play pla kol = playMove pla (getTheMoveP pla kol) kol
   
playMove prevpla plas kol = do
    let pla = head plas
    if null plas then
        if kol == Biale then -- wygraly czarne
            putStrLn $ "KONIEC, wygraly CZARNE\n" ++ toStr prevpla
        else -- wygraly biale
            putStrLn $ "KONIEC, wygraly BIALE\n" ++ toStr prevpla
    else
        play pla (inverse kol)