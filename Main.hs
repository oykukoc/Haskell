module Main where

import Data.Char
import Data.Maybe

import System.Console.Haskeline hiding (Settings)
import System.Console.ANSI
import System.IO(hFlush, stdout)
import Control.Concurrent(threadDelay)

import Othello
import AI


main :: IO ()
main = gameMenu

{--
Eingaben:
    None
Berechnet:
    Zeigt das Spielmenü an und wartet auf die Eingabe des Benutzers. Je nach Auswahl startet das entsprechende Spiel.
    Es gibt die Optionen, entweder mit zwei Spielern, gegen MiniMax oder gegen Alpha-Beta-Suche zu spielen.
    Bei einer ungültigen Eingabe wird eine Fehlermeldung angezeigt und das Menü erneut angezeigt.
--}
gameMenu :: IO ()
gameMenu = do
  clearScreen
  putStrLn "Willkommen bei Othello!"
  putStrLn "1 - Spiel zweier Tastaturspieler:innen"
  putStrLn "2 - Spiel mit Schwarz (o) gegen MiniMax"
  putStrLn "3 - Spiel mit Weiß (x) gegen MiniMax"
  putStrLn "4 - Spiel mit Schwarz (o) gegen Alpha-Beta-Suche"
  putStrLn "5 - Spiel mit Weiß (x) gegen Alpha-Beta-Suche"
  putStrLn "q - Beenden"
  putStr "> "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> startGame terminalPlayer terminalPlayer
    "2" -> startGame terminalPlayer (aiPlayer selectMoveMiniMax)
    "3" -> startGame (aiPlayer selectMoveMiniMax) terminalPlayer
    "4" -> startGame terminalPlayer (aiPlayer selectMoveAlphaBeta)
    "5" -> startGame (aiPlayer selectMoveAlphaBeta) terminalPlayer
    "q" -> putStrLn "Spiel beendet. Auf Wiedersehen!"
    _   -> do
      putStrLn "Ungültige Eingabe. Bitte erneut versuchen."
      gameMenu


{--
Eingaben:
    player1: Die Funktion, die für den ersten Spieler (Spieler 1) verwendet wird.
    player2: Die Funktion, die für den zweiten Spieler (Spieler 2) verwendet wird.
Berechnet:
    Initialisiert das Spielbrett und startet das Spiel, indem es die playGame-Funktion mit den gegebenen Parametern aufruft.
--}
startGame :: (Board -> Player -> IO (Maybe Piece)) -> (Board -> Player -> IO (Maybe Piece)) -> IO ()
startGame player1 player2 = do
  let board = initialBoard 8
  playGame board Light player1 player2


{--
Eingaben:
    board: Das aktuelle Spielbrett.
    currentPlayer: Der Spieler, der am Zug ist.
    player1: Die Funktion des ersten Spielers.
    player2: Die Funktion des zweiten Spielers.
Berechnet:
    Führt das Spiel aus, indem es den aktuellen Zustand des Spiels überprüft, das Brett rendert und den nächsten Zug ausführt.
    Wenn das Spiel vorbei ist, wird der Gewinner angezeigt oder es wird ein Unentschieden erklärt.
    Wenn das Spiel nicht vorbei ist, wird der Zug des aktuellen Spielers ausgeführt und das Spiel fortgesetzt.
--}
playGame :: Board -> Player -> (Board -> Player -> IO (Maybe Piece)) -> (Board -> Player -> IO (Maybe Piece)) -> IO ()
playGame board currentPlayer player1 player2 = do
  clearScreen
  putStrLn $ render board
  if over board currentPlayer
    then case winner board of
           Just p  -> putStrLn $ "Spiel beendet! Gewinner: " ++ show p
           Nothing -> putStrLn "Spiel beendet! Unentschieden!"
    else do
      putStrLn $ show currentPlayer ++ " ist am Zug."
      move <- if currentPlayer == Light then player1 board currentPlayer else player2 board currentPlayer
      case move of
        Just p ->
          case makeMove board p of
            Just newBoard -> do
              threadDelay 1000000
              playGame newBoard (otherP currentPlayer) player1 player2
            Nothing -> do
              putStrLn "Ungültiger Zug. Bitte erneut versuchen."
              playGame board currentPlayer player1 player2
        Nothing -> do
          putStrLn "Keine gültigen Züge verfügbar. Runde übersprungen."
          playGame board (otherP currentPlayer) player1 player2

{--
Eingaben:
    board: Das aktuelle Spielbrett.
    player: Der Spieler, der einen Zug macht.
Berechnet:
    Fragt den Benutzer nach einem Zug, indem die Eingabe von zwei Zeichen erwartet wird (z.B. "a 3").
    Überprüft, ob der Zug gültig ist, und gibt den Zug zurück, wenn er gültig ist. Andernfalls wird der Benutzer aufgefordert, es erneut zu versuchen.
--}
terminalPlayer :: Board -> Player -> IO (Maybe Piece)
terminalPlayer board player = do
  putStr "> "
  hFlush stdout
  x <- getChar
  putStr " "
  y <- getChar
  _ <- getLine
  let pos = (digitToInt y, ord x - ord 'a' + 1)
  if legal board (pos, player)
    then return $ Just (pos, player)
    else do
      putStrLn "Ungültiger Zug. Bitte erneut versuchen."
      terminalPlayer board player


{--
Eingaben:
    aiFunc: Die Funktion, die für den AI-Entscheidungsprozess verwendet wird (z.B. MiniMax oder Alpha-Beta-Suche).
    board: Das aktuelle Spielbrett.
    player: Der Spieler, der einen Zug machen muss.
Berechnet:
    Lässt den Computer seinen Zug basierend auf der AI-Funktion ausführen. Gibt den Zug zurück, wenn er gültig ist, andernfalls Nothing.
--}
aiPlayer :: (Board -> Player -> Int -> Maybe Piece) -> Board -> Player -> IO (Maybe Piece)
aiPlayer aiFunc board player = do
  let move = aiFunc board player 5
  case move of
    Just p  -> do
      putStrLn $ "Computer zieht: " ++ show (fst p)
      return $ Just p
    Nothing -> return Nothing