module SortCheck where

import Data.Array
import Data.List(delete)

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances.Array()

{--
Eingaben:
    arr: Ein Array von beliebigen indizierten Werten vom Typ e (der Typ der Elemente muss ein Ord-Instanz sein)

Berechnet:
    Gibt True zurück, wenn das Array arr in aufsteigender Reihenfolge sortiert ist, andernfalls False.
--}
isSorted :: (Ix i, Integral i, Ord e) => Array i e -> Bool
isSorted arr = all (\(i, j) -> arr ! i <= arr ! j) (zip indices (tail indices))
  where
    indices = range (bounds arr)

{--
Eingaben:
    array1: Ein Array von beliebigen indizierten Werten vom Typ e
    array2: Ein weiteres Array von beliebigen indizierten Werten vom Typ e

Berechnet:
    Gibt True zurück, wenn die Grenzen (Bounds) von array1 und array2 gleich sind, andernfalls False.
--}
isSameRange :: (Ix i, Integral i, Ord e) => Array i e -> Array i e -> Bool
isSameRange array1 array2 = bounds array1 == bounds array2

{--
Eingaben:
    array1: Ein Array von beliebigen indizierten Werten vom Typ e
    array2: Ein weiteres Array von beliebigen indizierten Werten vom Typ e

Berechnet:
    Gibt True zurück, wenn array1 und array2 die gleichen Elemente enthalten (ungeachtet der Reihenfolge),
    andernfalls False.
--}
sameElements :: (Ix i, Integral i, Ord e) => Array i e -> Array i e -> Bool
sameElements array1 array2 = checkElements (elems array1) (elems array2)
  where
    checkElements [] [] = True
    checkElements (e:es) remaining
      | e `elem` remaining = checkElements es (delete e remaining)
      | otherwise          = False
    checkElements _ _ = False

{--
Eingaben:
    f: Eine Funktion, die ein Array vom Typ Array Int Int akzeptiert und ein Array vom gleichen Typ zurückgibt.
    name: Ein String, der den Namen des zu testenden Algorithmus darstellt.

Berechnet:
    Führt drei Tests auf eine gegebene Sortierfunktion aus:
    1. Überprüft, ob das Array nach der Sortierung sortiert ist.
    2. Überprüft, ob das Array nach der Sortierung die gleichen Grenzen hat wie das Original-Array.
    3. Überprüft, ob das Array nach der Sortierung die gleichen Elemente wie das Original-Array enthält.
--}
useSortCheck :: (Array Int Int -> Array Int Int) -> String -> TestTree
useSortCheck f name = testGroup ("SortCheck " ++ name) [
  QC.testProperty "is sorted" $
    \array -> let sortedArray = f array
              in isSorted sortedArray,
  QC.testProperty "is same range" $
    \array -> let sortedArray = f array
              in isSameRange array sortedArray,
  QC.testProperty "is same elements" $
    \array -> let sortedArray = f array
              in sameElements array sortedArray
  ]
