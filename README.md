# Haskell-Warcaby

## TL;DR
### Jak działa
Projekt utknął na etapie, gdy program może grać sam ze sobą, czyli nie można brać aktywnego udziału w rozgrywce, a tylko ją oglądać. Po każdym ruchu wyświetlana jest plansza, a program szuka najlepszego ruchu dla aktualnie grającego koloru. Zaimplementowana została obsługa królówek.

### Jak uruchomić
Uruchamianie (ghci)
```bash
> cd src
src> ghci
Prelude> :load Main
```
Uruchamianie (cabal)
```bash
> cabal sandbox init
> cabal configure
> cabal build
> cabal run
```

Aby zmienić planszę, trzeba dokonać zmian w:

* `Game.hs` zaraz po importach znajduje się kilka plansz startowych, `initB` ładowana do `pl` to zwykła plansza początkowa
* `Main.hs` podajemy planszę, od której rozpocznie się rozgrywka, zaczynają białe

``` haskell
main = gameStartAuto pl --inicjuje rozgrywkę z planszą pl (initB)
```

Ogólnie w plikach:

* `Game.hs` - plansze i uruchamianie gry
* `Generation.hs` - szukanie najlepszej decyzji
* `Kills.hs` - szukanie zabójstw
* `Main.hs` - START!
* `Moves.hs` - szukanie ruchów
* `Stangry.hs` - deklaracje typów i większość funkcji zmieniających i wyciągających informacje o planszy

---

## Plansza
Istotne jest ułożenie, ponieważ czarne mają bazę u góry, a białe na dole planszy.
```
  12345678 -->x
1 .b.b.b.b
2 b.b.b.b.
3 .b.b.b.b
4 ........
5 ........
6 w.w.w.w.
7 .w.w.w.w
8 w.w.w.w.
|
V
y
```
* `w` - biały pionek
* `W` - biała królówka
* `b` - czarny pionek
* `B` - czarna królówka

## Decydowanie o ruchu

### Poszukiwanie zabójstw (Kills.hs)
`getKillBestSingle::Plansza->Kolor->[Plansza]` szuka wszystkich zabójstw możliwych do wykonania w tej turze dla zadanej planszy (stanu gry) oraz koloru, który właśnie wykonuje ruch oraz wybiera to nalepsze. Przez najlepsze rozumiem, zgodnie z zasadami gry, to które zabija najwięcej figur przeciwnika.
### Poszukiwanie przemieszczeń (Moves.hs)
`getMovesAllBrds::Plansza->Kolor->[Plansza]` zwraca listę plansz będących wynikami każdego możliwego w tej turze ruchu.
### Decyzja (Generation.hs)
`getTheMoveP::Plansza->Kolor->[Plansza]` znajduje najlepszy do wykonania ruch jaki udało się obliczyć.   
Poszukiwanie rozwiązania odbywa się poprzez generowanie kolejnych możliwych konsekwencji wykonania przemieszczenia, bo jeśli można zabić to musimy zabić, więc nie szukamy najlepszej decyzji za pomocą drzewa. Całość polega na wygenerowaniu możliwych przemieszczeń dla w danej turze, a następnie dla każdego z nich dalsze poszukiwanie najlepszych ruchów (na przemian białe/czarne), tak jakby drzewo, ale w rzeczywistości przechowywany jest jedynie najgłębszy poziom, bo tylko on jest istotny. Warunkami końca poszukiwania są:

* znalezienie planszy, która oznacza czyjąś wygraną (bo dalej nie ma czego szukać)
* osiągnięcie maksymalnej głębokości poszukiwania (`1000`)
* przekroczenie limitu elementów na poziomie drzewa - daje to przyjemną elastyczność w czasie poszukiwania, ponieważ im mniej możliwości na początku tym głębiej szukamy

Po wygenerowaniu wybieramy najlepszą decyzję spośród wygenerowanych, kryteriami są:

* głębokość rozwiązania - istotna, gdy wybieramy spośród gałęzi zakończonych naszą wygraną, wygrywa oczywiście najkrótsza
* stan planszy - jeśli nie widzimy wygranej to wybieramy ruch dający nam najlepszą wynikową sytuację na planszy, liczebność figur

Na ich podstawie wybierana jest najlepsza możliwość i wykonywany jest ruch. W praktyce oznacza to zwrócenie planszy, ponieważ cały czas operuję na planszach, a nie pojedynczych ruchach.
