module Bubblesort where

import Data.Array

{--
Eingaben:
    index1: Der Index des ersten Elements, das getauscht werden soll.
    index2: Der Index des zweiten Elements, das getauscht werden soll.
    array: Das Array, in dem die Elemente getauscht werden.

Berechnet:
    Gibt ein neues Array zurück, bei dem die Elemente an den Indizes index1 und index2 getauscht wurden.
--}
swap :: (Ix i, Integral i) => i -> i -> Array i e -> Array i e
swap index1 index2 array = array // [(index1, array ! index2), (index2, array ! index1)]

{--
Eingaben:
    lower: Der Index des ersten Elements, das in der Bubble Pass Iteration betrachtet wird.
    upper: Der Index des letzten Elements, das in der Bubble Pass Iteration betrachtet wird.
    array: Das Array, in dem die Elemente nach dem Bubble Sort Prinzip vertauscht werden.

Berechnet:
    Führt einen einzigen "Bubble Pass" durch, bei dem benachbarte Elemente im Array vertauscht werden, wenn das aktuelle Element größer ist als das nächste.
    Gibt das modifizierte Array nach der Durchführung des Bubble Pass zurück.
--}
bubblePass :: (Ix i, Integral i, Ord e) => i -> i -> Array i e -> Array i e
bubblePass lower upper array = foldl (\arr idx -> if arr ! idx > arr ! (idx + 1) then swap idx (idx + 1) arr else arr) array [lower .. upper - 1]

{--
Eingaben:
    lower: Der Index des ersten Elements im Array.
    upper: Der Index des letzten Elements im Array.
    array: Das Array, das mit dem Bubble Sort Algorithmus sortiert werden soll.

Berechnet:
    Führt den Bubble Sort Algorithmus auf dem Array durch, indem wiederholt benachbarte Elemente verglichen und getauscht werden, bis das gesamte Array sortiert ist.
    Die Rekursion endet, wenn der `lower`-Index gleich oder größer als der `upper`-Index ist.
    Gibt das sortierte Array zurück.
--}
bubblesort :: (Ix i, Integral i, Ord e) => i -> i -> Array i e -> Array i e
bubblesort lower upper array
  | lower >= upper = array
  | otherwise = bubblesort lower (upper - 1) (bubblePass lower upper array)

{--
Eingaben:
    array: Das Array, das sortiert werden soll.

Berechnet:
    Ruft die Funktion `bubblesort` auf, um das Array zu sortieren. Die Funktion `bounds` wird verwendet, um die unteren und oberen Indizes des Arrays zu erhalten, die als Eingabewerte an `bubblesort` übergeben werden.
    Gibt das sortierte Array zurück.
--}
sort :: (Ix i, Integral i, Ord e) => Array i e -> Array i e
sort array = let (lower, upper) = bounds array in bubblesort lower upper array
