module Stangry where
    
data Kolor = Biale | Czarne | Brak
    deriving (Show)
data TypNaDanymPolu = Pionek | Damka | Wolne
    deriving (Show, Eq)
data Pole = Pole { x :: Int, y :: Int, typ :: TypNaDanymPolu, kolor :: Kolor }
--   deriving (Show)
instance Show Pole where
    show Pole{kolor = Biale, typ = Pionek} = show 'w'
    show Pole{kolor = Biale, typ = Damka} = show "W"
    show Pole{kolor = Czarne, typ = Pionek} = show "b"
    show Pole{kolor = Czarne, typ = Damka} = show "B"
    show Pole{} = show " "
    
--plansza to [[Pole]]
data Plansza = Plansza {
    plansza :: [[Pole]],
    numB :: Int,
    numW :: Int
    } deriving (Show)
    
--pokaz plansze
pokazPlansze pl= mapM print (plansza pl)