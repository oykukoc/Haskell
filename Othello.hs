module Othello where

import Data.Bifunctor
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Game

data Player = Light | Dark deriving (Eq, Ord, Show)

type Position = (Int, Int)

type Piece = (Position, Player)

data Board = Board
  { size :: Int,
    pieces :: Map.Map Position Player
  }
  deriving (Eq, Show)

otherP :: Player -> Player
otherP Light = Dark
otherP Dark = Light

playerLabel :: Player -> Char
playerLabel Dark = 'o'
playerLabel Light = 'x'

posRange :: Board -> [Int]
posRange b = [1 .. size b]


render :: Board -> String
render board = unlines $ header ++ rows ++ [footer]
  where
    size = posRange board
    header = ["   | " ++ intercalate " | " (map (:[]) ['a'..'h']) ++ " |"]
    divider = "---+" ++ concat (replicate (length size) "---+") ++ "---"
    footer = header !! 0
    rows = [divider] ++ [renderRow r | r <- size] ++ [divider]

    renderRow r = show r ++ " | " ++ intercalate " | " [pieceAt (r, c) | c <- size] ++ " | " ++ show r
    pieceAt pos = maybe " " (pure . playerLabel) (Map.lookup pos (pieces board))


{--
Eingaben:
    size: Die gewünschte Größe des Spielbretts.
Berechnet:
    Erstellt ein neues, leeres Spielbrett mit der gegebenen Größe.
    Wenn die Größe ungültig ist (weniger als 4 oder ungerade), wird ein Fehler geworfen.
    Ansonsten werden die Anfangssteine an den richtigen Positionen platziert.
--}
initialBoard :: Int -> Board
initialBoard size
  | size < 4 || odd size = error "Board size must be even and at least 4x4"
  | otherwise = Board size initialPieces
  where
    mid = size `div` 2
    initialPieces = Map.fromList [
        ((mid, mid), Light),
        ((mid + 1, mid + 1), Light),
        ((mid, mid + 1), Dark),
        ((mid + 1, mid), Dark)
      ]

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der Spieler, der den Zug ausführen möchte (Position und Spielerfarbe).
Berechnet:
    Führt den Zug aus und gibt ein neues Spielbrett zurück, bei dem der Stein des Spielers auf der gegebenen Position platziert wird.
--}
justMakeMove :: Board -> Piece -> Board
justMakeMove b (pos, player) = b {pieces = Map.insert pos player (pieces b)}

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der Spieler und die Position des Spielsteins, der platziert werden soll.
Berechnet:
    Überprüft, ob der Zug legal ist. Falls ja, wird der Zug auf dem Spielbrett ausgeführt und zurückgegeben.
    Wenn der Zug nicht legal ist, wird Nothing zurückgegeben.
--}
makeMove :: Board -> Piece -> Maybe Board
makeMove b p
  | legal b p = Just $ foldl flipPiece (justMakeMove b p) (affectedPositions b p)
  | otherwise = Nothing

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der Spieler und die Position des Spielsteins, der auf seine Legalität überprüft wird.
Berechnet:
    Gibt True zurück, wenn der Zug legal ist, und False, wenn er illegal ist.
    Ein Zug ist legal, wenn die Position nicht bereits belegt ist und wenn er Auswirkungen auf das Spiel hat (also eine gegnerische Stellung umgedreht wird).
--}
legal :: Board -> Piece -> Bool
legal b (pos, player) =
  pos `notElem` Map.keys (pieces b)
    && not (null (affectedPositions b (pos, player)))

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der Spieler, der die möglichen Züge machen kann.
Berechnet:
    Gibt eine Liste aller legalen Züge zurück, die der gegebene Spieler auf dem aktuellen Spielbrett ausführen kann.
--}
legalMoves :: Board -> Player -> [Piece]
legalMoves b p = [(pos, p) | pos <- allPositions b, legal b (pos, p)]

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der aktuelle Spieler.
Berechnet:
    Gibt True zurück, wenn das Spiel für den gegebenen Spieler vorbei ist, d.h., der Spieler keine Züge mehr machen kann.
--}
over :: Board -> Player -> Bool
over b p = null (legalMoves b p) && null (legalMoves b (otherP p))

{--
Eingaben:
    b: Das aktuelle Spielbrett.
Berechnet:
    Bestimmt den Gewinner des Spiels basierend auf der Anzahl der Steine jedes Spielers. Wenn beide Spieler gleich viele Steine haben, gibt es keinen Gewinner.
--}
winner :: Board -> Maybe Player
winner b =
  let scores = Map.elems (pieces b)
      lightScore = length (filter (== Light) scores)
      darkScore = length (filter (== Dark) scores)
   in case compare lightScore darkScore of
        GT -> Just Light
        LT -> Just Dark
        EQ -> Nothing

{--
Eingaben:
    b: Das aktuelle Spielbrett.
Berechnet:
    Gibt eine Liste aller möglichen Positionen auf dem Spielbrett zurück.
--}
allPositions :: Board -> [Position]
allPositions b = [(r, c) | r <- posRange b, c <- posRange b]

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der Spieler und die Position des Spielsteins, dessen Wirkung auf benachbarte Steine überprüft werden soll.
Berechnet:
    Gibt eine Liste der Positionen zurück, die durch das Setzen eines Steins auf die gegebene Position beeinflusst werden.
--}
affectedPositions :: Board -> Piece -> [Position]
affectedPositions b (pos, player) = concatMap (checkDirection b pos player) directions

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    pos: Die Position des Steins, dessen Farbe geändert werden soll.
Berechnet:
    Kehrt die Farbe des Spielsteins an der gegebenen Position um.
--}
flipPiece :: Board -> Position -> Board
flipPiece b pos =
  case Map.lookup pos (pieces b) of
    Just currentPlayer ->
      b {pieces = Map.adjust (const (otherP currentPlayer)) pos (pieces b)}
    Nothing -> b

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    pos: Die aktuelle Position eines Spielsteins.
    player: Der Spieler, der den Zug machen möchte.
    direction: Eine Richtung, in der nach benachbarten Steinen des Gegners gesucht wird.
Berechnet:
    Überprüft die benachbarten Positionen in der gegebenen Richtung und gibt alle Positionen zurück, die durch den Zug betroffen sind.
--}
checkDirection :: Board -> Position -> Player -> (Int, Int) -> [Position]
checkDirection b (r, c) player (dr, dc) = go (r + dr, c + dc) []
  where
    go pos acc = case Map.lookup pos (pieces b) of
      Just p | p == otherP player -> go (fst pos + dr, snd pos + dc) (pos : acc)
      Just p | p == player -> acc
      _ -> []

{--
Berechnet:
    Gibt alle Richtungen zurück, in denen benachbarte Steine überprüft werden müssen. Es sind 8 mögliche Richtungen, die in Form von Tupeln (dr, dc) angegeben werden.
--}
directions :: [(Int, Int)]
directions = [(dr, dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]