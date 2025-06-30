module Mat where

-- Zum Warmwerden
{--
Eingaben:
    n: Eine ganze Zahl, deren Quersumme berechnet werden soll.

Berechnet die Quersumme der Zahl `n`, indem die einzelnen Ziffern summiert werden.
    - Wenn `n` gleich 0 ist, wird 0 zurückgegeben.
    - Andernfalls wird die letzte Ziffer von `n` (n `mod` 10) zur Quersumme der restlichen Ziffern addiert.
--}
quersumme :: Integer-> Integer
quersumme n
  | n == 0 = 0
  | otherwise = (abs n `mod` 10) + quersumme (abs n `div` 10)

{--
Eingaben:
    i: Ein ganzzahliger Faktor, der mit jeder Ziffer multipliziert wird.
    n: Eine ganze Zahl, deren spezielle Ziffernsumme berechnet werden soll.

Berechnet die gewichtete Ziffernsumme der Zahl `n`, wobei jede Ziffer mit dem Faktor `i` multipliziert wird.
    - Wenn `n` gleich 0 ist, wird 0 zurückgegeben.
    - Andernfalls wird die letzte Ziffer (`remaining`) von `n` mit `i` multipliziert und zur rekursiven Berechnung der restlichen Ziffern addiert.
--}
prfsum :: Int-> Integer-> Integer
prfsum i n
  | n == 0 = 0
  | otherwise = remaining * fromIntegral i + prfsum (stellen nextNumber) nextNumber
  where
    remaining = n `mod` 10
    nextNumber = n `div` 10

{--
Eingaben:
    i: Eine ganze Zahl, deren Stellenanzahl berechnet werden soll.

Gibt die Anzahl der Ziffern in der Zahl `i` zurück.
    - Wenn `i` gleich 0 ist, wird 0 zurückgegeben.
    - Andernfalls wird die Länge der Zeichenkette, die `i` darstellt, berechnet.
--}
stellen :: Integer-> Int
stellen i
  | i == 0 = 0
  | otherwise = length (show i)

{--
Eingaben:
    n: Eine ganze Zahl, für die die Prüfziffer berechnet werden soll.

Berechnet die Prüfziffer der Zahl `n` basierend auf einer speziellen Summenberechnung.
    - Wenn `n` gleich 0 ist, wird 0 zurückgegeben.
    - Andernfalls wird die Berechnung der Summenfunktion (`prfsum`) verwendet.
      Falls der Rest (`remainder`) 10 ist, wird 0 als Prüfziffer zurückgegeben, andernfalls der Rest selbst.
--}
pruefziffer :: Integer-> Int
pruefziffer n
  | n == 0 = 0
  | otherwise = if remainder == 10 then 0 else remainder
  where
    summe= fromIntegral (prfsum (stellen n) n)
    remainder = summe `mod` 11

{--
Eingaben:
    m: Eine ganze Zahl, die eine Matrikelnummer darstellt.

Überprüft, ob die letzte Ziffer der Matrikelnummer `m` mit der berechneten Prüfziffer übereinstimmt.
    - Wenn `m` gleich 0 ist, wird `False` zurückgegeben.
    - Andernfalls wird die letzte Ziffer (`lastDigit`) von `m` mit der berechneten Prüfziffer (`matrNo`) verglichen.
--}
checkMatrNo :: Integer-> Bool
checkMatrNo m
  | m == 0 = False
  | otherwise = lastDigit == matrNo
  where
    lastDigit = fromIntegral (m `mod` 10)
    matrNo = pruefziffer (m `div` 10)

