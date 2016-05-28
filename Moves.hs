module Moves where
import Stangry

import Control.Monad
-- MOVE

getMoveUL = getMove UL 
getMoveUR = getMove UR 
getMoveDL = getMove DL 
getMoveDR = getMove DR 

getMove kier pla x y = do
    let pl = plansza pla
    let el = getElemAt pl x y
    let nell = getElem kier pla x y
    when (null nell) []
    let nel = head nell
    let npla = moveF2F pla el nel
    if  typ nel == Wolne && kolor nel == Brak then
        if typ el == Damka then
        (nel,npla) : uncurry (getMove kier pla) (poleXY nel)
        else
        [(nel,npla)]
    else []

getMoves Pole{x=x,y=y,typ=Pionek,kolor=Biale} pla = getMoveUL pla x y ++ getMoveUR pla x y
getMoves Pole{x=x,y=y,typ=Pionek,kolor=Czarne} pla = getMoveDL pla x y ++ getMoveDR pla x y
getMoves Pole{x=x,y=y,typ=Damka} pla = getMoveUL pla x y ++ getMoveUR pla x y ++ getMoveDL pla x y ++ getMoveDR pla x y

getMovesP pla x y = do
    let el = getElemAt2 pla x y
    --el
    getMoves el pla
    
getMovesAll::Plansza->Kolor->[(Pole,Plansza)]
getMovesAll pla kol = do
    let pola = findByColor pla kol
    concatMap (`getMoves` pla) pola
    
getMovesAllBrds::Plansza->Kolor->[Plansza]
getMovesAllBrds pla kol = map (\x -> snd x) (getMovesAll pla kol)