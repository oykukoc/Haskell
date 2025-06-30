module Tri where 

{--
Eingaben:
    chr: Ein Zeichen, das wiederholt wird.
    number: Anzahl der Wiederholungen (>= 0).

Erzeugt einen String, der `chr` `number`-Mal wiederholt.
    - Gibt einen leeren String zurück, wenn `number` <= 0.
    - Ansonsten wird `chr` angehängt und die Funktion rekursiv mit 
      `number - 1` aufgerufen.
--}
chars :: Char-> Int-> String
chars chr number = if number <= 0 then "" else chr : chars chr (number - 1)

{--
Eingaben:
    chr: Ein Zeichen zur Darstellung des Dreiecks.
    number: Höhe des Dreiecks (>= 0).

Erzeugt einen String für ein linksbündiges Dreieck aus `chr` mit Höhe `number`.
    - Gibt einen leeren String zurück, wenn `number` <= 0 ist.
    - Ansonsten wird rekursiv das Dreieck erstellt, wobei jede Zeile mit
      `\n` endet.
--}
triL :: Char -> Int -> String
triL chr number = if number <= 0 then "" else triL chr (number - 1) ++ chars chr number ++ "\n"

{--
Eingaben:
    chr: Ein Zeichen für das Dreieck.
    number: Höhe des Dreiecks.

Erzeugt einen String für ein rechtsbündiges Dreieck aus `chr` mit Höhe `number`.
    - Leitet die Berechnung an die Hilfsfunktion `triCount` weiter.
--}
triR :: Char-> Int-> [Char]
triR chr number = triCount chr number 1

{--
Eingaben:
    chr: Ein Zeichen für das Dreieck.
    number: Höhe des Dreiecks.
    counter: Aktueller Zähler für die Zeilen.

Erzeugt einen String für ein rechtsbündiges Dreieck.
    - Gibt einen leeren String zurück, wenn der Zähler größer als die Höhe ist.
    - Fügt Leerzeichen und `chr` hinzu und ruft sich rekursiv mit 
      `counter + 1` auf.
--}
triCount :: Char -> Int -> Int -> String
triCount chr number counter  = if number < counter then "" else chars ' ' (number - counter) ++ chars chr counter ++ "\n" ++ triCount chr number (counter + 1)
