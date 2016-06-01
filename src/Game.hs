module Game where
    
import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad

import Stangry
import Moves
import Kills
import Generation

initB = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
pl = parseGame initB
init2 = ".b.b.b.b\n........\n.b.b.b..\n........\n.....b..\nW.w.w.w.\n...w.w.w\nw.w.w.w."
pl2 = parseGame init2
init3 = "........\n........\n........\n........\n.....b..\nW.w.w.w.\n...w.w.w\nw.w.w.w."
pl3 = parseGame init3
init4 = ".w.w....\n........\n........\n........\n.....b..\nW.w.w.w.\n...w.w.w\nw.w.w.w."
pl4 = parseGame init4
init5 = ".b.b.b.b\n........\n.b.b.b..\n........\n.b...b..\nW.w.w.w.\n...w.w.w\nw.w.w.w."
pl5 = parseGame init5

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

-- blowLine line = filter (/= "") (splitOn ""  line)--(\x -> x /= "")

parseGame l = do
    let rows = zip [1..] $ map (zip [1..]) $ splitOn "\n" l
    let pl = map (\y -> map (\x -> parseElemXY (snd x) (fst x) (fst y) ) $ snd y) rows
    Plansza pl (length $ findByColorRun pl Czarne) (length $ findByColorRun pl Biale)
    
    
gameStart pla = play pla Biale
    
play pla kol = playMove pla (getTheMoveP pla kol) kol
   
playMove prevpla plas kol = do
    let pla = head plas
    putStrLn $ "Teraz graja: " ++ show kol ++ "\n" ++ toStr prevpla
    if null plas then
        if kol == Biale then -- wygraly czarne
            putStrLn $ "KONIEC, wygraly CZARNE\n" ++ toStr prevpla
        else -- wygraly biale
            putStrLn $ "KONIEC, wygraly BIALE\n" ++ toStr prevpla
    else
        play pla (inverse kol)