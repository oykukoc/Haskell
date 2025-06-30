module Klima where

-- Diese Zeile darf nicht verändert werden
-- Sie verhindert den Einsatz konkreter Funktionen,
-- die ihr direkt oder indirekt selbst implementieren sollt

import Data.Maybe (mapMaybe)
import Klimadaten
import Text.Read (readMaybe)
import Prelude hiding (average, length, maximum, minimum)

-- Speichert eine Wettermessung an einem Ort im Format:
-- Klimawert Land Stadt Monat Temperatur Niederschlag
data Klimawert = Klimawert
  { land :: String,
    stadt :: String,
    monat :: Monat,
    temperatur :: Double,
    niederschlag :: Double
  }
  deriving (Eq, Show)

data Monat = Jan | Feb | Mar | Apr | Mai | Jun | Jul | Aug | Sep | Okt | Nov | Dez
  deriving (Read, Eq, Show, Enum)

monate :: [Monat]
monate = [Jan .. Dez]

-----------------

{--
Eingabe:
    daten: Ein String mit Klimadaten in Zeilen, getrennt durch Kommata.

Ausgabe:
    [Klimawert]: Liste der erfolgreich geparsten Klimawerte.

Beschreibung:
    `einlesen` verarbeitet Klimadaten aus einem String, zerlegt sie zeilenweise und 
    analysiert jede Zeile mit der Hilfsfunktion `toParcel`. 
    Ungültige Zeilen werden ignoriert.

Hilfsfunktion:
    `toParcel` zerlegt eine Zeile in Bestandteile und wandelt sie sicher in einen `Klimawert` um.
--}
einlesen :: String -> [Klimawert]
einlesen daten =
  mapMaybe toParcel (lines daten)
  where
    toParcel :: String -> Maybe Klimawert
    toParcel parselLine =
      case words (filter (/= ',') parselLine) of
        [land, stadt, monatToStr, tempToStr, niedToStr] -> do
          monat <- readMaybe monatToStr
          temperatur <- readMaybe tempToStr
          niederschlag <- readMaybe niedToStr
          return $ Klimawert land stadt monat temperatur niederschlag
        _ -> Nothing

{--
Eingabe:
    str: Name des Landes oder der Stadt, für die die minimale Temperatur ermittelt werden soll.
    vals: Eine Liste von Klimawerten.

Ausgabe:
    Maybe Double: Die minimale Temperatur für die übereinstimmenden Werte, falls vorhanden, sonst `Nothing`.

Beschreibung:
    `minTemp` filtert die Liste der Klimawerte nach dem angegebenen Land oder der Stadt.
    Es extrahiert die Temperaturwerte aus den gefilterten Klimawerten.
    Wenn keine Werte gefunden werden, gibt die Funktion `Nothing` zurück.
    Andernfalls wird der kleinste Temperaturwert mithilfe von `foldr min` berechnet.
--}
minTemp :: String -> [Klimawert] -> Maybe Double
minTemp str vals =
  let filteredTemp = filter (\valueOfDatas -> land valueOfDatas == str || stadt valueOfDatas == str) vals
      temperaturValues = map temperatur filteredTemp
   in if null temperaturValues
        then Nothing
        else Just (foldr min (head temperaturValues) (tail temperaturValues))

{--
Eingabe:
    str: Name des Landes oder der Stadt, für die der maximale Niederschlag ermittelt werden soll.
    vals: Eine Liste von Klimawerten.

Ausgabe:
    Maybe Double: Der maximale Niederschlag für die übereinstimmenden Werte, falls vorhanden, sonst `Nothing`.

Beschreibung:
    `maxNieder` filtert die Liste der Klimawerte nach dem angegebenen Land oder der Stadt.
    Es extrahiert die Niederschlagswerte aus den gefilterten Klimawerten.
    Wenn keine Werte gefunden werden, gibt die Funktion `Nothing` zurück.
    Andernfalls wird der größte Niederschlagswert mithilfe von `foldr max` berechnet.
--}
maxNieder :: String -> [Klimawert] -> Maybe Double
maxNieder str vals =
  let filteredNieder = filter (\valueOfDatas -> land valueOfDatas == str || stadt valueOfDatas == str) vals
      niederValues = map niederschlag filteredNieder
   in if null filteredNieder
        then Nothing
        else Just (foldr max (head niederValues) (tail niederValues))

{--
Eingabe:
    str: Name der Stadt, für die die Durchschnittswerte berechnet werden sollen.
    mon: Der Monat, für den die Werte berechnet werden sollen.
    vals: Eine Liste von Klimawerten.

Ausgabe:
    (Int, Int): Ein Tupel, das den Durchschnitt der Temperatur und des Niederschlags für die angegebenen Filterkriterien enthält.
    Wenn keine Werte gefunden werden, gibt die Funktion `(0, 0)` zurück.

Beschreibung:
    `durschnittKlima` filtert die Liste der Klimawerte nach der angegebenen Stadt und dem Monat.
    Es extrahiert die Temperatur- und Niederschlagswerte aus den gefilterten Klimawerten.
    Wenn keine Werte gefunden werden, wird `(0, 0)` zurückgegeben.
    Ansonsten werden die Durchschnittswerte berechnet und auf ganze Zahlen gerundet.
--}
durschnittKlima :: String -> Monat -> [Klimawert] -> (Int, Int)
durschnittKlima str mon vals =
  let filteredDrKlima = filter (\valueOfDatas -> stadt valueOfDatas == str && monat valueOfDatas == mon) vals

      temperaturValues = map temperatur filteredDrKlima
      niederValues = map niederschlag filteredDrKlima

      (avgTemp, avgNieder) =
        if null temperaturValues
          then (0, 0)
          else
            let totalTemp = foldr (+) 0 temperaturValues
                totalNieder = foldr (+) 0 niederValues
                count = foldr (\_ acc -> acc + 1) 0 temperaturValues
             in (round (totalTemp / fromIntegral count), round (totalNieder / fromIntegral count))
   in (avgTemp, avgNieder)

-----------

-- Speichert Klimainformationen zu einem Ort im Format:
-- Stadtklima Stadt [(Monat, Temperatur, Niederschlag)]
data Stadtklima = Stadtklima
  { ort :: String,
    daten :: [(Monat, Int, Int)]
  }
  deriving (Show, Eq)

{--
Eingabe:
    str: Name der Stadt, für die das Klima erstellt werden soll.
    vals: Eine Liste von Klimawerten.

Ausgabe:
    `Stadtklima`: Eine Datenstruktur, die die durchschnittliche Temperatur und den Niederschlag pro Monat für die angegebene Stadt enthält.

Beschreibung:
    `klima` berechnet das Klima für eine angegebene Stadt. Es iteriert über alle Monate des Jahres,
    berechnet die durchschnittliche Temperatur und den durchschnittlichen Niederschlag für jeden Monat,
    und speichert diese Werte in einer Liste von Monat-Daten-Paaren.
    Diese Liste wird dann zusammen mit dem Stadtnamen in der `Stadtklima`-Struktur zurückgegeben.
--}
klima :: String -> [Klimawert] -> Stadtklima
klima str vals =
  let monatData = map (\monat -> (monat, durschnittTemp monat, durschnittNiederschlag monat)) monate

      durschnittTemp :: Monat -> Int
      durschnittTemp monat = fst (durschnittKlima str monat vals)

      durschnittNiederschlag :: Monat -> Int
      durschnittNiederschlag monat = snd (durschnittKlima str monat vals)
   in Stadtklima str monatData

{--
Eingabe:
    stadt: Ein `Stadtklima`, das Klimadaten für eine Stadt enthält.

Ausgabe:
    Eine Liste von Monaten, in denen Vegetationsbedingungen erfüllt sind.

Beschreibung:
    `vegetation` überprüft die Klimadaten einer Stadt und gibt alle Monate zurück,
    in denen die durchschnittliche Temperatur mindestens 5°C beträgt und der durchschnittliche Niederschlag
    mindestens das Doppelte der Temperatur ist. Diese Bedingungen entsprechen typischen Vegetationsanforderungen.
--}
vegetation :: Stadtklima -> [Monat]
vegetation stadt =
  let validMonths =
        filter
          ( \(_, temperaturValues, niederValues) ->
              temperaturValues >= 5 && niederValues >= 2 * temperaturValues
          )
          (daten stadt)
   in map (\(monat, _, _) -> monat) validMonths
