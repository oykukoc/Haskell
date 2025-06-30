{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AI (selectMoveMiniMax, selectMoveAlphaBeta) where

import qualified AlphaBeta
import qualified Data.Map as Map
import Data.Maybe
import Game
import qualified MiniMax
import Othello

data State = State
  { board :: Board,
    nextPlayer :: Player,
    depth :: Int
  }

data Action = Action {piece :: Piece} deriving (Ord, Eq, Show)

instance Game State Action where
  {--
  Eingaben:
      s: Der aktuelle Zustand des Spiels.
  Berechnet:
      Bestimmt, ob das Spiel im aktuellen Zustand vorbei ist, basierend auf dem aktuellen Spieler.
      Das Spiel ist vorbei, wenn der aktuelle Spieler keine gültigen Züge mehr hat.
  --}
  terminal :: State -> Bool
  terminal s = over (board s) (nextPlayer s)

  {--
  Eingaben:
      s: Der aktuelle Zustand des Spiels.
  Berechnet:
      Gibt den Utility-Wert des Spiels zurück:
      - Wenn ein Gewinner ermittelt wurde, gibt es 1, wenn der Gewinner der aktuelle Spieler ist, und -1, wenn der Gegner gewonnen hat.
      - Wenn kein Gewinner vorliegt, gibt es den Wert der Heuristik für das gegebene Spielbrett zurück.
  --}
  utility :: State -> Double
  utility s =
    let b = board s
        p = nextPlayer s
     in case winner b of
          Just w -> if w == p then 1 else -1
          Nothing -> heuristic b p

  {--
  Eingaben:
      s: Der aktuelle Zustand des Spiels, der das Spielbrett, den nächsten Spieler und die Suchtiefe enthält.
  Berechnet:
      Gibt eine Liste von Aktionen (Zügen) zurück, die der aktuelle Spieler im aktuellen Zustand des Spiels ausführen kann.
      Eine Aktion wird durch das Platzieren eines Spielsteins an einer gültigen Position dargestellt.
  --}
  actions :: State -> [Action]
  actions s = map Action (legalMoves (board s) (nextPlayer s))

  {--
  Eingaben:
      s: Der aktuelle Zustand des Spiels.
      a: Die Aktion, die ausgeführt werden soll.
  Berechnet:
      Gibt den neuen Zustand des Spiels zurück, der das Ergebnis der ausgeführten Aktion darstellt.
      Der neue Zustand beinhaltet das aktualisierte Spielbrett, den Wechsel des Spielers und die erhöhte Suchtiefe.
  --}
  result :: State -> Action -> State
  result s (Action p) =
    case makeMove (board s) p of
      Just b -> State b (otherP (nextPlayer s)) (depth s + 1)
      Nothing -> s

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der Spieler, für den die Heuristik berechnet wird.
Berechnet:
    Berechnet eine Heuristik für das gegebene Spielbrett, basierend auf der Anzahl der eigenen und gegnerischen Spielsteine.
    Die Heuristik ist die Differenz zwischen der Anzahl der Spielsteine des Spielers und der Anzahl der Spielsteine des Gegners, normalisiert durch die Gesamtzahl der Felder auf dem Spielbrett.
--}
heuristic :: Board -> Player -> Double
heuristic b p =
  let myPieces = length $ filter (\(_, player) -> player == p) (Map.toList (pieces b))
      opponentPieces = length $ filter (\(_, player) -> player == otherP p) (Map.toList (pieces b))
   in fromIntegral (myPieces - opponentPieces) / fromIntegral (size b * size b)

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der Spieler, der den Zug machen soll.
    d: Die maximale Suchtiefe für den MiniMax-Algorithmus.
Berechnet:
    Wendet den MiniMax-Algorithmus auf das gegebene Spielbrett an und wählt den besten Zug für den Spieler p aus.
    Gibt den ausgewählten Zug als Piece zurück, falls ein gültiger Zug gefunden wird.
    Wenn keine gültigen Züge gefunden werden, gibt es Nothing zurück.
--}
selectMoveMiniMax :: Board -> Player -> Int -> Maybe Piece
selectMoveMiniMax b p d =
  case fst (MiniMax.minimax d (State b p 0)) of
    (Action a : _) -> Just a
    _ -> Nothing

{--
Eingaben:
    b: Das aktuelle Spielbrett.
    p: Der Spieler, der den Zug machen soll.
    d: Die maximale Suchtiefe für den Alpha-Beta-Algorithmus.
Berechnet:
    Wendet den Alpha-Beta-Algorithmus auf das gegebene Spielbrett an und wählt den besten Zug für den Spieler p aus.
    Gibt den ausgewählten Zug als Piece zurück, falls ein gültiger Zug gefunden wird.
    Wenn keine gültigen Züge gefunden werden, gibt es Nothing zurück.
--}
selectMoveAlphaBeta :: Board -> Player -> Int -> Maybe Piece
selectMoveAlphaBeta b p d =
  case fst (AlphaBeta.alphabeta d (State b p 0)) of
    (Action a : _) -> Just a
    _ -> Nothing