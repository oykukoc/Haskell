module Pie where

{--
Eingaben:
    i: Eine Zahl größer oder gleich 0.

Fehler:
    Falls i negativ ist, wird ein Fehler ausgelöst.

Berechnet den Wert jedes Terms in der Pi-Serie.
    - Der Term wird als 1 / (2 * i + 1) berechnet.
    - `fromIntegral` wird verwendet, um `i` von Int zu Double zu konvertieren,
      damit die Division ein genaues Ergebnis liefert.
--}
sumd  :: Int-> Double
sumd i = if i < 0 then error "sumd: Eingabe darf nicht negativ sein." else  1/(2 * fromIntegral i  + 1)

{--
Eingaben:
    i: Eine beliebige Zahl.

Fehler:
    Falls i negativ ist, wird ein Fehler ausgelöst.

Berechnet die Summe der Pi-Serie bis zu einem bestimmten Term.
    - Wenn i gleich 0 ist, wird 1 zurückgegeben.
    - Bei geraden i wird der Wert von sumd i positiv addiert.
    - Bei ungeraden i wird der Wert von sumd i negativ subtrahiert.
--}
qpie :: Int-> Double
qpie i
  | i < 0 = error "qpie: Eingabe darf nicht negativ sein."
  | i == 0 = 1
  | even i = qpie (i-1) + sumd i
  | otherwise = qpie (i-1) - sumd i

{--
Eingaben:
    i: Eine Zahl größer gleich 0.

Fehler:
    Falls i negativ ist, wird ein Fehler ausgelöst.
    
Berechnet den approximativen Wert von Pi, indem die Summe der Pi-Serie 
mit 4 multipliziert wird.
--}
pie :: Int-> Double
pie i =  if i < 0 then error "pie: Eingabe darf nicht negativ sein." else 4 * qpie i
