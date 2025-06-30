{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Battleship where

import Data.List
import Data.Maybe (fromMaybe)

data Board = Board
  { dimX :: Int,
    dimY :: Int,
    ships :: [Ship]
  }
  deriving (Show, Eq)

type Coord = (Int, Int)

type Ship = [Coord]

s0 = [(1, 1), (1, 2)] :: [(Int, Int)]

s1 = [(3, 0)]

s2 = [(3, 2)]

s3 = [(4, 4)]

b1 = Board 5 5 [s0, s1, s2, s3]

b3 = Board 3 3 [s0]

b4 = Board 2 2 [[(1, 0), (1, 1)]]

data Answer = Wasser | Treffer | Versenkt deriving (Show, Eq)

{--
Eingabe:
    board: Ein `Board`, das das Spielfeld mit den Koordinaten und Schiffen enthält.
    coord: Ein `Coord`, das die angegriffene Koordinate repräsentiert.

Beschreibung:
    Die Funktion `probe` führt einen Angriff auf das gegebene Spielfeld durch und prüft,
    ob das angegriffene Schiff getroffen wurde oder nicht. Wenn ein Schiff getroffen wurde, 
    wird die angegriffene Koordinate aus dem Schiff entfernt und überprüft, ob das Schiff versenkt wurde.
    - Wenn das Schiff keine Koordinaten mehr hat, wird es aus der Schiffs-Liste entfernt und als "versenkt" markiert.
    - Wenn das Schiff noch Koordinaten hat, wird es als "getroffen" markiert.
--}
probe :: Board -> Coord -> (Board, Answer)
probe (Board coordX coordY ships) coord =
  case filter (elem coord) ships of
    [] -> (Board coordX coordY ships, Wasser)
    (s : ss) ->
      let newShip = removeCoord coord s
          newShips =
            if null newShip
              then removeShip s ships
              else newShip : removeShip s ships
       in (Board coordX coordY newShips, if null newShip then Versenkt else Treffer)
  where
    removeCoord :: Coord -> Ship -> Ship
    removeCoord c = filter (/= c)

    removeShip :: Ship -> [Ship] -> [Ship]
    removeShip s = filter (/= s)

data GameState = GameState
  { board :: Board,
    moves :: [Coord],
    prefMoves :: [Coord],
    movesDone :: [Coord],
    currentMove :: Coord
  }
  deriving (Show, Eq)

{--
Eingabe:
    board: Ein `Board`, das das Spielfeld mit den Dimensionen `coordX` und `coordY` enthält.

Beschreibung:
    Die Funktion `allCoords` erzeugt eine Liste von allen möglichen Koordinaten im Spielfeld.
    Dabei wird für jede `x`-Koordinate im Bereich von `0` bis `coordX - 1` und jede `y`-Koordinate 
    im Bereich von `0` bis `coordY - 1` eine Kombination gebildet. 
--}
allCoords :: Board -> [Coord]
allCoords (Board coordX coordY _) = [(x, y) | x <- [0 .. coordX - 1], y <- [0 .. coordY - 1]]

gs1 = GameState b1 (allCoords b1) [] [] (0, 0)

gs3 = GameState b3 (allCoords b3) [] [] (0, 0)

gs4 = GameState b4 (allCoords b4) [] [] (0, 0)

{--
Eingabe:
    gameState: Ein `GameState`-Wert, der das Spielfeld (`Board`), alle Züge (`moves`), die bevorzugten Züge (`prefMoves`),
               die bereits gemachten Züge (`movesDone`) und den aktuellen Zug (`currentMove`) enthält.

Beschreibung:
    Die Funktion `printGameState` gibt das Spielfeld als Textdarstellung aus. Für jedes Feld auf dem Spielfeld
    wird ein Zeichen gewählt, das den Status des Feldes widerspiegelt:
    - `'c'` für das aktuelle Feld (`currentMove`),
    - `'p'` für bevorzugte Züge (`prefMoves`),
    - `'x'` für bereits gemachte Züge (`movesDone`),
    - `'@'` für Felder, die Schiffe enthalten,
    - `'.'` für leere Felder.
    Das Spielfeld wird von unten nach oben ausgegeben.
--}
printGameState :: GameState -> String
printGameState (GameState (Board dimX dimY ships) moves prefMoves movesDone currentMove) =
  unlines [[cell (x, y) | x <- [0 .. dimX - 1]] | y <- reverse [0 .. dimY - 1]]
  where
    cell coord
      | coord == currentMove = 'c'
      | coord `elem` prefMoves = 'p'
      | coord `elem` movesDone = 'x'
      | any (coord `elem`) ships = '@'
      | otherwise = '.'

data Game a = Move a [Game a] | End a deriving (Show, Eq)

{--
Eingabe:
    gs: Ein `GameState`-Wert, der den aktuellen Zustand des Spiels mit den Informationen zum Spielfeld (Board), den möglichen Zügen (moves), den bevorzugten Zügen (prefMoves), den bereits gemachten Zügen (movesDone) und dem aktuellen Zug (currentMove) enthält.

Beschreibung:
    Die Funktion `playGame` simuliert das weitere Spielen eines Spiels und erzeugt eine rekursive Struktur von Spielzuständen. Sie prüft, ob das Spiel vorbei ist oder fortgesetzt werden kann, und erstellt eine Liste von nachfolgenden Spielzuständen basierend auf den möglichen Zügen.
    
1. Wenn das Spielfeld keine Schiffe mehr enthält (also das Spiel zu Ende ist), wird der aktuelle Zustand als Endzustand (`End gs`) zurückgegeben.
2. Andernfalls wird aus den möglichen Zügen (`moves` oder `prefMoves`) eine Liste von nachfolgenden Zuständen erzeugt:
    - Jeder mögliche Zug wird in einen neuen Zustand umgewandelt und rekursiv mit `playGame` bearbeitet.

    Das erzeugte Spiel wird in einem `Move`-Konstrukt zurückgegeben, das den aktuellen Zustand (`gs`) sowie die Liste der nachfolgenden Spielzustände enthält.
--}
playGame :: GameState -> Game GameState
playGame gs@(GameState b moves prefMoves movesDone currentMove)
  | null (ships b) = End gs
  | otherwise =
      let possibleMoves = if null prefMoves then moves else prefMoves
          nextStates = [makeNextState m | m <- possibleMoves]
       in Move gs (map playGame nextStates)
  where
    makeNextState m =
      let (newBoard, answer) = probe b m
          (newMoves, newPrefMoves) = updateMovesAndPrefs newBoard moves prefMoves movesDone m answer
          newMovesDone = movesDone ++ [m]
       in GameState newBoard newMoves newPrefMoves newMovesDone m


{--
validCoord :: Board -> Coord -> Bool

Eingabe:
    b: Ein `Board`-Wert, das die Dimensionen des Spielfelds (maxX, maxY) enthält.
    c: Ein `Coord`-Wert, das eine Position auf dem Spielfeld angibt.

Beschreibung:
    Die Funktion `validCoord` prüft, ob die angegebenen Koordinaten `c` innerhalb der Grenzen des Spielfelds `b` liegen. Sie gibt `True` zurück, wenn die Koordinaten gültig sind, andernfalls `False`.
--}
validCoord :: Board -> Coord -> Bool
validCoord (Board maxX maxY _) (x, y) = x >= 0 && y >= 0 && x < maxX && y < maxY

{--
Eingabe:
    b: Ein `Board`-Wert.
    coord: Ein `Coord`-Wert, der die Koordinaten einer Position auf dem Spielfeld angibt.

Beschreibung:
    Die Funktion `straightNeighbors` gibt eine Liste der benachbarten Felder zurück, die sich direkt horizontal oder vertikal (keine Diagonale) zum angegebenen Feld befinden. Die Nachbarn müssen gültige Koordinaten haben.
--}
straightNeighbors :: Board -> Coord -> [Coord]
straightNeighbors b (x, y) =
  [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)], validCoord b (x + dx, y + dy)]

{--
Eingabe:
    b: Ein `Board`-Wert.
    coord: Ein `Coord`-Wert, der die Koordinaten einer Position auf dem Spielfeld angibt.

Beschreibung:
    Die Funktion `diagonalNeighbors` gibt eine Liste der benachbarten Felder zurück, die sich diagonal zum angegebenen Feld befinden. Auch hier müssen die Koordinaten gültig sein.
--}
diagonalNeighbors :: Board -> Coord -> [Coord]
diagonalNeighbors b (x, y) =
  [(x + dx, y + dy) | dx <- [-1, 1], dy <- [-1, 1], validCoord b (x + dx, y + dy)]

{--
Eingabe:
    coord: Ein `Coord`-Wert, das die Position auf dem Spielfeld angibt.
    b: Ein `Board`-Wert, das eine Liste von Schiffen enthält.

Beschreibung:
    Die Funktion `shipContaining` gibt das Schiff zurück, das die angegebene Position `coord` enthält. Falls kein Schiff diese Position enthält, wird eine leere Liste zurückgegeben.
--}
shipContaining :: Coord -> Board -> Ship
shipContaining c (Board _ _ ships) = fromMaybe [] (find (c `elem`) ships)

{--
Eingabe:
    b: Ein `Board`-Wert.
    sh: Ein `Ship`-Wert, das eine Liste von Koordinaten eines Schiffes enthält.

Beschreibung:
    Die Funktion `surroundingCoords` gibt alle benachbarten Felder zurück, die sich sowohl in gerader Linie als auch diagonal um das angegebene Schiff befinden. Dabei wird die Gültigkeit der Koordinaten geprüft.
--}
surroundingCoords :: Board -> Ship -> [Coord]
surroundingCoords b sh =
  nub $ concatMap (filter (validCoord b) . (\coord -> straightNeighbors b coord ++ diagonalNeighbors b coord)) sh

{--
Eingabe:
    c: Ein `Coord`-Wert.
    cs: Eine Liste von `Coord`-Werten.

Beschreibung:
    Die Funktion `removeCoord` entfernt das gegebene `Coord` aus der Liste `cs` und gibt die bereinigte Liste zurück.
--}
removeCoord :: Coord -> [Coord] -> [Coord]
removeCoord c = filter (/= c)

{--
Eingabe:
    cs: Eine Liste von `Coord`-Werten, die entfernt werden sollen.
    ms: Eine Liste von `Coord`-Werten, aus der die Koordinaten entfernt werden.

Beschreibung:
    Die Funktion `removeCoords` entfernt alle Koordinaten aus der Liste `ms`, die in der Liste `cs` enthalten sind, und gibt die bereinigte Liste zurück.
--}
removeCoords :: [Coord] -> [Coord] -> [Coord]
removeCoords cs ms = foldr removeCoord ms cs

{--
Eingabe:
    b: Ein `Board`-Wert.
    moves: Eine Liste von Koordinaten, die die verbleibenden Züge darstellen.
    prefMoves: Eine Liste von bevorzugten Zügen.
    movesDone: Eine Liste von bereits ausgeführten Zügen.
    m: Ein `Coord`-Wert, der den aktuellen Zug darstellt.
    ans: Ein `Answer`-Wert, der das Ergebnis des Zuges darstellt.

Beschreibung:
    Die Funktion `updateMovesAndPrefs` aktualisiert die Listen `moves` und `prefMoves` basierend auf dem Ergebnis des Zuges (`ans`). Je nach Ergebnis (`Wasser`, `Treffer`, `Versenkt`) werden die Listen entsprechend angepasst.
--}
updateMovesAndPrefs :: Board -> [Coord] -> [Coord] -> [Coord] -> Coord -> Answer -> ([Coord], [Coord])
updateMovesAndPrefs b moves prefMoves movesDone m ans = case ans of
  Wasser ->
    let newMoves = removeCoord m moves
        newPrefMoves = removeCoord m prefMoves
     in (newMoves, newPrefMoves)
  Treffer ->
    let newMoves = removeCoord m moves
        newPrefMovesBase = removeCoord m prefMoves
        nbrs = [n | n <- straightNeighbors b m, n `notElem` movesDone, n `elem` newMoves]
        newPrefMoves = newPrefMovesBase ++ nbrs
     in (newMoves, newPrefMoves)
  Versenkt ->
    let sh = shipContaining m b
        surr = surroundingCoords b sh
        newMoves = removeCoords (m : surr) moves
        newPrefMoves = []
     in (newMoves, newPrefMoves)

{--
Functor Instanziierung für den Typ Game:

Der Funktor `fmap` wird verwendet, um eine Funktion `f` auf die enthaltenen Werte des `Game`-Typs anzuwenden.

1. Im Fall von `Move x gs`:
    - `f` wird auf den Wert `x` angewendet.
    - Danach wird `fmap f` auf jedes Element der Liste `gs` angewendet, um die enthaltenen `Game`-Werte ebenfalls zu transformieren.
    - Dies bedeutet, dass wir alle verschachtelten `Game`-Instanzen in der Liste ebenfalls transformieren.

2. Im Fall von `End x`:
    - `f` wird direkt auf den Wert `x` angewendet.

Der Funktor `fmap` verändert den Inhalt eines `Game`-Werts, ohne die Struktur von `Game` zu verändern.
--}
instance Functor Game where
  fmap f (Move x gs) = Move (f x) (map (fmap f) gs)
  fmap f (End x) = End (f x)

{--
Eingabe:
    game: Ein Wert des Typs `Game GameState`, der entweder ein `End`-Zustand oder ein `Move`-Zustand mit einer Liste von nachfolgenden Spielzuständen enthält.

Beschreibung:
    Die Funktion `moveRest` konvertiert ein Spiel (vom Typ `Game GameState`) in eine neue Darstellung, bei der jeder Spielzustand als Zeichenkette (String) angegeben wird. Die Darstellung enthält den aktuellen Zug sowie die Gesamtzahl der Züge.

1. Im Fall von `Move gs gsList`:
    - Der aktuelle Zustand (`gs`) wird in einen String umgewandelt, der die Form `"currentMove/totalMoves"` hat, wobei `currentMove` der aktuelle Zug und `totalMoves` die Gesamtzahl der Züge ist.
    - Für jeden nachfolgenden Spielzustand (`gsList`) wird `moveRest` rekursiv aufgerufen, um die entsprechenden Strings zu erstellen.

2. Im Fall von `End gs`:
    - Der Endzustand (`gs`) wird ebenfalls in die Form `"currentMove/totalMoves"` umgewandelt.
--}
moveRest :: Game GameState -> Game String
moveRest (Move gs gsList) =
  Move (show (currentMove gs) ++ "/" ++ show (length (moves gs))) (map moveRest gsList)
moveRest (End gs) =
  End (show (currentMove gs) ++ "/" ++ show (length (moves gs)))

{--
Eingabe:
    f: Eine Funktion, die ein Element vom Typ `a` und eine Liste von Werten vom Typ `[b]` als Eingabe erhält und einen Wert vom Typ `b` zurückgibt.
    g: Eine Funktion, die ein Element vom Typ `a` als Eingabe erhält und einen Wert vom Typ `b` zurückgibt.
    game: Ein Wert vom Typ `Game a`, der entweder ein `Move` oder ein `End` sein kann.

Beschreibung:
Die Funktion `foldGame` faltet rekursiv den `Game`-Typ und wendet die Funktionen `f` und `g` an:

1. Bei `Move x gs`:
    - `foldGame` wird rekursiv auf jedes Element in `gs` angewendet.
    - `f` wird auf `x` und das Ergebnis der Rekursion angewendet.

2. Bei `End x`:
    - `g` wird direkt auf `x` angewendet, da dies das Ende der Struktur ist.
--}
foldGame :: (a -> [b] -> b) -> (a -> b) -> Game a -> b
foldGame f g (Move x gs) = f x (map (foldGame f g) gs)
foldGame f g (End x) = g x

{--
Eingabe:
    game: Ein Wert des Typs `Game a`, der entweder ein `Move` oder ein `End` ist.

Beschreibung:
    Die Funktion `countNodes` zählt die Anzahl der Knoten in einem Spielbaum:
    - Wenn es sich um einen `Move` handelt, wird der Knoten selbst gezählt (daher 1), und dann wird rekursiv
      die Anzahl der Knoten für alle möglichen Spielzustände (dargestellt durch `gs`) in der Liste summiert.
    - Wenn es sich um ein `End` handelt, wird ebenfalls 1 gezählt, da es sich um einen Endzustand handelt.
--}
countNodes :: Game a -> Int
countNodes (Move _ gs) = 1 + sum (map countNodes gs)
countNodes (End _) = 1

{--
Eingabe:
    game: Ein Wert des Typs `Game a`, der entweder ein `End`-Zustand oder ein `Move`-Zustand mit einer Liste von nachfolgenden Spielzuständen enthält.

Beschreibung:
    Die Funktion `allPaths` gibt alle möglichen Pfade zurück, die zu einem Endzustand führen. Sie funktioniert rekursiv und geht dabei wie folgt vor:

1. Im Fall von `Move x gs`:
    - Für jeden nachfolgenden Spielzustand (`gs`) wird `allPaths` rekursiv aufgerufen, um alle Pfade für diesen Zustand zu finden.
    - Für jeden gefundenen Pfad wird der aktuelle Zustand (`x`) an den Anfang des Pfades angefügt.

2. Im Fall von `End x`:
    - Ein Endzustand ist erreicht, daher wird eine Liste mit diesem Zustand zurückgegeben.
--}
allPaths :: Game a -> [[a]]
allPaths (Move x gs) = concatMap (map (x :) . allPaths) gs
allPaths (End x) = [[x]]

{--
Eingabe:
    coords: Eine Liste von Koordinaten (`[Coord]`), die als Paare von x- und y-Werten dargestellt sind.
    n: Eine Ganzzahl (`Int`), die angibt, in welchem Abstand Koordinaten ausgewählt werden sollen.

Beschreibung:
    Die Funktion `everyNthCoord` wählt alle n-ten Koordinaten aus der Liste `coords` aus:
    - Zuerst wird mit `zip coords [1..]` jeder Koordinate ein Index zugewiesen.
    - Dann werden nur die Koordinaten ausgewählt, deren Index (`index`) ein Vielfaches von `n` ist (also `index mod n == 0`).
--}
everyNthCoord :: [Coord] -> Int -> [Coord]
everyNthCoord coords n = [coord | (coord, index) <- zip coords [1..], index `mod` n == 0]

{--
Eingabe:
    game: Ein Wert des Typs `Game GameState`, der entweder ein `End`-Zustand oder ein `Move`-Zustand mit einer Liste von nachfolgenden Spielzuständen enthält.

Beschreibung:
    Die Funktion `shortestGames` berechnet alle kürzesten Pfade, die zu einem Endzustand führen, indem sie rekursiv die Liste von möglichen nachfolgenden Zuständen untersucht.
    Sie geht dabei wie folgt vor:

1. Im Fall von `End gs`:
    - Ein Endzustand ist erreicht. Daher wird eine Liste mit diesem Spielzustand zurückgegeben.

2. Im Fall von `Move gs gsList`:
    - Für jede der möglichen nachfolgenden Spielzustände (`gsList`) wird `shortestGames` rekursiv angewendet, um alle möglichen Pfade zu finden.
    - Wenn keine Pfade gefunden werden, gibt die Funktion eine leere Liste zurück. Andernfalls werden die gefundenen Pfade mit dem aktuellen Zustand (`gs`) kombiniert.
--}
shortestGames :: Game GameState -> [[GameState]]
shortestGames (End gs) = [[gs]]
shortestGames (Move gs gsList) =
  let paths = concatMap shortestGames gsList
   in if null paths then [] else map (gs :) paths

gs4Show =
  ( GameState
      { board = Board {dimX = 5, dimY = 5, ships = [[(1, 1)], [(3, 2)], [(4, 4)]]},
        moves = [],
        prefMoves = [(1, 1)],
        movesDone = [(3, 0), (4, 1), (2, 1), (3, 1), (4, 0), (2, 0), (1, 2), (2, 3), (0, 3), (0, 1), (2, 2), (0, 2), (1, 3)],
        currentMove = (1, 3)
      }
  )