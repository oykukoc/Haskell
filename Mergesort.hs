module Mergesort where

import Data.Array

{--
Eingaben:
    array1: Das erste Array, das mit dem zweiten Array zusammengeführt werden soll.
    array2: Das zweite Array, das mit dem ersten Array zusammengeführt werden soll.

Berechnet:
    Führt zwei sortierte Arrays (`array1` und `array2`) zusammen, indem die Elemente der Arrays in einer sortierten Reihenfolge kombiniert werden.
    Wenn eines der Arrays leer ist, wird das andere Array zurückgegeben.
    Gibt das zusammengeführte Array zurück.
--}
merge :: (Ix i, Ord e) => Array i e -> Array i e -> Array i e
merge array1 array2
  | null (elems array1) = array2 
  | null (elems array2) = array1 
  | otherwise = array (min lower1 lower2, max upper1 upper2) mergedElems
  where
    (lower1, upper1) = bounds array1
    (lower2, upper2) = bounds array2 
    mergedElems = zip (range (min lower1 lower2, max upper1 upper2)) (mergeLists (elems array1) (elems array2))

    mergeLists :: Ord e => [e] -> [e] -> [e]
    mergeLists [] ys = ys
    mergeLists xs [] = xs
    mergeLists (leftElem:leftList) (rightElem:rightList)
      | leftElem <= rightElem    = leftElem : mergeLists leftList (rightElem:rightList)
      | otherwise                = rightElem : mergeLists (leftElem:leftList) rightList

{--
Eingaben:
    lower: Das untere Ende des Indexbereichs des Arrays.
    high: Das obere Ende des Indexbereichs des Arrays.
    arr: Das Array, das sortiert werden soll.

Berechnet:
    Sortiert ein Array unter Verwendung des Mergesort-Algorithmus. Der Arraybereich wird rekursiv in zwei Hälften unterteilt, bis die Bereiche nur noch ein Element enthalten.
    Diese Teilarrays werden dann mit der `merge`-Funktion zusammengeführt, um das endgültige sortierte Array zu erhalten.
--}
mergesort :: (Ix i, Integral i, Ord e) => i -> i -> Array i e -> Array i e
mergesort lower high arr
  | lower > high = array (lower, high) []
  | lower == high = array (lower, high) [(lower, arr ! lower)]
  | otherwise = merge (mergesort lower mid arr) (mergesort (mid + 1) high arr)
  where
    mid = (lower + high) `div` 2

{--
Eingaben:
    array: Das Array, das sortiert werden soll.

Berechnet:
    Sortiert ein Array, indem es den Mergesort-Algorithmus aufruft. Falls das Array leer ist, wird es unverändert zurückgegeben. Andernfalls wird das Array mit den unteren und oberen Indizes an die `mergesort`-Funktion übergeben, um das Array zu sortieren.
--}
sort :: (Ix i, Integral i, Ord e) => Array i e -> Array i e
sort array
  | null (elems array) = array
  | otherwise = let (lower, upper) = bounds array in mergesort lower upper array
