module Average where 

{--
  Eingaben:
      str: Eine Zeichenkette, die die zu berechnenden Zahlen in Form von Strings enthält.

  Berechnet den Durchschnitt der Zahlen in der Zeichenkette `str`.
      - Wenn `str` leer ist, wird 0 zurückgegeben.
      - Andernfalls wird die Summe (`total`) und die Anzahl (`count`) der Zahlen berechnet.
        Der Durchschnitt wird als die Summe geteilt durch die Anzahl zurückgegeben.
--}
average :: String-> Double
average str
  | str == [] = 0
  | otherwise = fromIntegral total / fromIntegral count
  where
    (total, count) = averageSumAndCount str

{--
Eingaben:
    chr: Ein Zeichen, nach dem gesucht wird.
    str: Eine Zeichenkette, in der nach `chr` gesucht wird.

Gibt den Teil der Zeichenkette `str` vor dem ersten Auftreten von `chr` zurück.
    - Wenn `str` leer ist, wird eine leere Zeichenkette zurückgegeben.
    - Wenn das erste Zeichen von `str` gleich `chr` ist, wird eine leere Zeichenkette zurückgegeben.
    - Ansonsten wird das erste Zeichen von `str` zur Rückgabe hinzugefügt und die Funktion wird rekursiv mit dem Rest von `str` (ohne das erste Zeichen) aufgerufen.
--}
before :: Char-> String-> String
before _ [] = []
before chr str
  | head str == chr = []
  | otherwise = head str : before chr (tail str)

{--
Eingaben:
    chr: Ein Zeichen, nach dem gesucht wird.
    str: Eine Zeichenkette, in der nach `chr` gesucht wird.

Gibt den Teil der Zeichenkette `str` nach dem ersten Auftreten von `chr` zurück.
    - Wenn `str` leer ist, wird eine leere Zeichenkette zurückgegeben.
    - Wenn das erste Zeichen von `str` gleich `chr` ist, wird der Rest von `str` (nach `chr`) zurückgegeben.
    - Ansonsten wird die Funktion rekursiv mit dem Rest von `str` (ohne das erste Zeichen) aufgerufen.
--}
after :: Char-> String-> String
after _ [] = []
after chr str
  | head str == chr = tail str
  | otherwise = after chr (tail str)

{--
Eingaben:
    str: Eine Zeichenkette, die aus durch Semikolons getrennten Ganzzahlen besteht.

Berechnet die Summe und die Anzahl der Ganzzahlen in `str`.
    - Wenn `str` leer ist, wird ein Tupel (0, 0) zurückgegeben, was die Summe und die Anzahl repräsentiert.
    - Andernfalls wird der Wert vor dem ersten Semikolon (durch die Funktion `before` erhalten) als erste Zahl gelesen und zur Summe hinzugefügt.
    - Die Funktion `after` wird verwendet, um den Rest der Zeichenkette nach dem ersten Semikolon zu erhalten.
    - Die Funktion ruft sich rekursiv auf, um den Rest der Zahlen zu verarbeiten und aggregiert die Summe und die Anzahl.
--}
averageSumAndCount :: String -> (Int, Int)
averageSumAndCount [] = (0, 0)
averageSumAndCount str = (firstPartValue + totalValue, counter + 1)
  where
    firstPartValue = read (before ';' str) :: Int
    remainingPart = after ';' str
    (totalValue, counter) = averageSumAndCount remainingPart
    