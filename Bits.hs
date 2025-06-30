{-# LANGUAGE InstanceSigs #-}

module Bits where

data BitSeq = I BitSeq | O BitSeq | E

{--
Eingabe:
    bitSeq: Eine Bit-Sequenz vom Typ `BitSeq`, für die die Länge berechnet werden soll.

Berechnet die Länge einer gegebenen Bit-Sequenz.
    - Wenn die Eingabe `E` (leere Sequenz) ist, wird die Länge als `0` zurückgegeben.
    - Wenn die Eingabe mit `I` beginnt, wird `1` zur Länge der restlichen Sequenz addiert.
    - Wenn die Eingabe mit `O` beginnt, wird ebenfalls `1` zur Länge der restlichen Sequenz addiert.
    - Die Funktion ruft sich rekursiv auf, bis die gesamte Sequenz verarbeitet wurde.
--}
bitLength :: BitSeq -> Int
bitLength E = 0
bitLength (I bitSeq) = 1 + bitLength bitSeq
bitLength (O bitSeq) = 1 + bitLength bitSeq

{--
Eingabe:
    bitSeq: Eine Bit-Sequenz (`BitSeq`), deren Länge angepasst werden soll.
    anzahl: Die gewünschte Länge der Bit-Sequenz.

Passt die Länge einer Bit-Sequenz auf die gewünschte Länge (`anzahl`) an.
    - Kürzt die Sequenz, wenn sie länger ist.
    - Ergänzt `O`, wenn sie kürzer ist.
    - Gibt sie unverändert zurück, wenn die Längen übereinstimmen.

Hilfsfunktionen:
    - `bitSeqLength`: Berechnet die Länge der Sequenz.
    - `getRest`: Entfernt das erste Bit aus der Sequenz.
--}
toNumericLength :: BitSeq -> Int -> BitSeq
toNumericLength E _ = O E
toNumericLength bitSeq anzahl
  | anzahl <= 0 = E
  | anzahl < bitSeqLength = toNumericLength (getRest bitSeq) anzahl
  | anzahl > bitSeqLength = toNumericLength (O bitSeq) anzahl
  | otherwise = bitSeq
  where
    bitSeqLength = bitLength bitSeq
    getRest (I rest) = rest
    getRest (O rest) = rest
    getRest E = E

{--
Eingabe:
    firstBS: Die erste Bit-Sequenz (`BitSeq`), deren Länge angepasst werden soll.
    secondBS: Die zweite Bit-Sequenz (`BitSeq`), deren Länge angepasst werden soll.

Gleicht die Längen zweier Bit-Sequenzen an:
    - Wenn die erste Sequenz kürzer ist, wird sie mit `toNumericLength` auf die Länge der zweiten Sequenz erweitert.
    - Wenn die zweite Sequenz kürzer ist, wird sie mit `toNumericLength` auf die Länge der ersten Sequenz erweitert.
    - Sind die Sequenzen bereits gleich lang, werden sie unverändert zurückgegeben.
--}
makeBitLengthEqual :: BitSeq -> BitSeq -> (BitSeq, BitSeq)
makeBitLengthEqual firstBS secondBS
  | firstLength < secondLength = (toNumericLength firstBS secondLength, secondBS)
  | firstLength > secondLength = (firstBS, toNumericLength secondBS firstLength)
  | otherwise = (firstBS, secondBS)
  where
    firstLength = bitLength firstBS
    secondLength = bitLength secondBS

{--
Eingabe:
    - Eine `BitSeq`, die als String dargestellt werden soll.

Funktion:
    - `E`: Wird als leerer String dargestellt.
    - `I`: Fügt '1' vor der String-Darstellung der restlichen Bit-Sequenz hinzu.
    - `O`: Fügt '0' vor der String-Darstellung der restlichen Bit-Sequenz hinzu.
--}
instance Show BitSeq where
  show :: BitSeq -> String
  show E = ""
  show (I bitSeq) = '1' : show bitSeq
  show (O bitSeq) = '0' : show bitSeq

{--
Eingabe:
    - Zwei `BitSeq`-Werte (`a` und `b`), die auf Gleichheit geprüft werden sollen.

Funktion:
    - `makeBitLengthEqual` wird verwendet, um die beiden Sequenzen auf die gleiche Länge zu bringen.
    - Die Hilfsfunktion `equalTo` prüft die Gleichheit rekursiv:
        - Zwei `E`-Sequenzen sind gleich.
        - Zwei `I`-Sequenzen sind gleich, wenn ihre Restsequenzen gleich sind.
        - Zwei `O`-Sequenzen sind gleich, wenn ihre Restsequenzen gleich sind.
        - Jede andere Kombination ist ungleich.
--}
instance Eq BitSeq where
  (==) :: BitSeq -> BitSeq -> Bool
  a == b = equalTo firstBS secondBS
    where
      (firstBS, secondBS) = makeBitLengthEqual a b
      equalTo E E = True
      equalTo (I frs) (I snd) = equalTo frs snd
      equalTo (O frs) (O snd) = equalTo frs snd
      equalTo _ _ = False

{--
Eingabe:
    - Zwei `BitSeq`-Werte (`a` und `b`), die auf ihre Ordnung geprüft werden sollen.

Funktion:
    - `makeBitLengthEqual` wird verwendet, um die beiden Sequenzen auf die gleiche Länge zu bringen.
    - Die Hilfsfunktion `compareTo` prüft rekursiv:
        - Zwei `E`-Sequenzen sind gleich und somit <=.
        - Zwei `I`-Sequenzen sind <=, wenn ihre Restsequenzen <= sind.
        - Zwei `O`-Sequenzen sind <=, wenn ihre Restsequenzen <= sind.
        - Eine `I`-Sequenz ist niemals <= einer `O`-Sequenz.
        - Eine `O`-Sequenz ist immer <= einer `I`-Sequenz.
--}
instance Ord BitSeq where
  (<=) :: BitSeq -> BitSeq -> Bool
  a <= b = compareTo firstBS secondBS
    where
      (firstBS, secondBS) = makeBitLengthEqual a b
      compareTo E E = True
      compareTo (I frs) (I snd) = compareTo frs snd
      compareTo (O frs) (O snd) = compareTo frs snd
      compareTo (I _) (O _) = False
      compareTo (O _) (I _) = True

{--
Eingaben:
    str: Eine Zeichenkette aus '0' und '1', die eine Bitfolge repräsentiert.

Fehler:
    - Falls die Zeichenkette ungültige Zeichen enthält.

Berechnet die BitSeq aus der Bitfolge.
    - Bei einer leeren Eingabe wird `E` zurückgegeben.
    - '0' wird zu `O`, '1' wird zu `I`.
--}
fromBitString :: String -> BitSeq
fromBitString "" = E
fromBitString ('0' : xs) = O (fromBitString xs)
fromBitString ('1' : xs) = I (fromBitString xs)
fromBitString _ = error "Invalid character"

{--
Eingaben:
    bitSeq: Ein BitSeq, dessen führende Nullen entfernt werden sollen.

Berechnung:
    - Wenn das BitSeq leer ist (`E`), wird es unverändert zurückgegeben.
    - Wenn das erste Bit eine `I` (1) ist, wird das BitSeq unverändert zurückgegeben.
    - Wenn das erste Bit eine `O` (0) ist, wird das erste Element entfernt und die Funktion rekursiv auf den Rest angewendet.
--}
removeLeadingZeros :: BitSeq -> BitSeq
removeLeadingZeros E = E
removeLeadingZeros (I bitSeq) = I bitSeq
removeLeadingZeros (O bitSeq) = O (removeLeadingZeros bitSeq)

{--
Eingaben:
    firstBS, secondBS: Zwei BitSeqs, die mit logischem "OR" verknüpft werden sollen.

Berechnung:
    - Zuerst werden die BitSeqs so angepasst, dass sie die gleiche Länge haben.
    - Dann wird für jedes Bit-Paar das logische "OR" berechnet:
        - Wenn beide Bits `1` (I) sind, wird `1` (I) zurückgegeben.
        - Wenn eines der Bits `0` (O) und das andere `1` (I) ist, wird `1` (I) zurückgegeben.
        - Wenn beide Bits `0` (O) sind, wird `0` (O) zurückgegeben.
        - Falls einer der BitSeqs leer ist, wird der andere zurückgegeben.
--}
lor :: BitSeq -> BitSeq -> BitSeq
lor firstBS secondBS = case (frs, snd) of
  (E, _) -> snd
  (_, E) -> frs
  (I frs, I snd) -> I $ lor frs snd
  (I frs, O snd) -> I $ lor frs snd
  (O frs, I snd) -> I $ lor frs snd
  (O frs, O snd) -> O $ lor frs snd
  where
    (frs, snd) = makeBitLengthEqual firstBS secondBS

{--
Eingaben:
    bs: Ein BitSeq, das invertiert (NOT) werden soll.

Berechnung:
    - Für jedes Bit in der BitSeq wird die "NOT"-Operation ausgeführt:
        - Wenn das Bit `1` (I) ist, wird es zu `0` (O).
        - Wenn das Bit `0` (O) ist, wird es zu `1` (I).
    - Das Ergebnis der "NOT"-Operation wird rekursiv aufgerufen, bis die gesamte BitSeq bearbeitet ist.
--}
lnot :: BitSeq -> BitSeq
lnot E = E
lnot (I bs) = O (lnot bs)
lnot (O bs) = I (lnot bs)

{--
Eingaben:
    firstBS: Das erste BitSeq für die "AND"-Operation
    secondBS: Das zweite BitSeq für die "AND"-Operation

Berechnung:
    - Die Funktion vergleicht jeweils die Bits der beiden BitSeqs:
        - Wenn beide Bits `1` sind (I), wird das Ergebnis `1` (I) sein.
        - Wenn eines der Bits `0` ist (O), wird das Ergebnis `0` (O) sein.
    - Die BitSeqs werden rekursiv von links nach rechts verglichen, bis das Ende erreicht ist.
--}
land :: BitSeq -> BitSeq -> BitSeq
land firstBS secondBS = case (frs, snd) of
  (E, _) -> frs
  (_, E) -> snd
  (I frs, I snd) -> I $ land frs snd
  (I frs, O snd) -> O $ land frs snd
  (O frs, I snd) -> O $ land frs snd
  (O frs, O snd) -> O $ land frs snd
  where
    (frs, snd) = makeBitLengthEqual firstBS secondBS

{--
Eingaben:
    bs: Ein BitSeq, das verschoben werden soll.
    n: Eine Zahl, die angibt, um wie viele Stellen das BitSeq nach rechts verschoben wird.

Berechnung:
    - Die Funktion verschiebt das BitSeq um `n` Positionen nach rechts.
    - Wenn `n` kleiner oder gleich der Länge des BitSeq ist, werden die ersten `n` Bits entfernt.
    - Falls `n` die Länge des BitSeq überschreitet, wird das Ergebnis `E` (leeres BitSeq) sein.
--}
lshr :: BitSeq -> Int -> BitSeq
lshr bs n
  | n <= 0 = bs
  | cnt <= n = E
  | otherwise = dropBit bs (cnt - n)
  where
    dropBit E _ = E
    dropBit _ 0 = E
    dropBit (I rest) n = I $ dropBit rest (n - 1)
    dropBit (O rest) n = O $ dropBit rest (n - 1)
    cnt = bitLength bs
    
{--
Eingaben:
    bs: Ein BitSeq, das verschoben werden soll.
    n: Eine Zahl, die angibt, um wie viele Stellen das BitSeq nach links verschoben wird.

Berechnung:
    - Die Funktion verschiebt das BitSeq um `n` Positionen nach links.
    - Bei jeder Verschiebung wird ein `0` an das Ende des BitSeq angehängt.
    - Die Verschiebung wird rekursiv durchgeführt, bis die gewünschte Anzahl erreicht ist.
--}
lshl :: BitSeq -> Int -> BitSeq
lshl E 0 = E
lshl E n = O $ lshl E (n - 1)
lshl (I rest) n = I $ lshl rest n
lshl (O rest) n = O $ lshl rest n