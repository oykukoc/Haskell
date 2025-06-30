{-# LANGUAGE EmptyDataDeriving #-}

module Sammy where

{--
Repräsentiert ein Sandwich mit einer Brotsorte, einer Liste von Füllungen, einer Liste von Beilagen und einer Liste von Kondimenten.

Eingaben:
    brot: Die Brotsorte des Sandwiches.
    fuellung: Eine Liste von Füllungen für das Sandwich (z. B. Bacon, Feta).
    beilagen: Eine Liste von Beilagen für das Sandwich (z. B. Pickels, Salat).
    kondimente: Eine Liste von Kondimenten für das Sandwich (z. B. Senf, Mayo).
--}
data Sandwich = Sandwich
  { brot :: Brot,
    fuellung :: [Fuellung], 
    beilagen :: [Beilage], 
    kondimente :: [Kondiment]
  }
  deriving (Show, Eq)

{--
Repräsentiert die Brotsorte, die für ein Sandwich verwendet werden kann.

Mögliche Werte:
    Roggenbrot: Roggenbrot als Brotsorte.
    Toastbrot: Toastbrot als Brotsorte.
--}
data Brot = Roggenbrot | Toastbrot
  deriving (Show, Eq)

{--
Repräsentiert die Füllung, die in ein Sandwich gelegt werden kann.

Mögliche Werte:
    Bacon: Bacon als Füllung.
    Feta: Feta als Füllung.
    RoastBeef: Roastbeef als Füllung.
    Cheddar: Cheddar als Füllung.
    Salami: Salami als Füllung.
--}
data Fuellung = Bacon | Feta | RoastBeef | Cheddar | Salami
  deriving (Show, Eq)

{--
Repräsentiert die Beilage, die zu einem Sandwich hinzugefügt werden kann.

Mögliche Werte:
    Pickels: Eingelegte Gurken als Beilage.
    Salat: Salat als Beilage.
    Gurken: Frische Gurken als Beilage.
    Tomaten: Tomaten als Beilage.
    Oliven: Oliven als Beilage.
--}
data Beilage = Pickels | Salat | Gurken | Tomaten | Oliven
  deriving (Show, Eq)

{--
Repräsentiert die verschiedenen Kondimente, die zu einem Sandwich hinzugefügt werden können.

Mögliche Werte:
    Senf: Senf als Kondiment.
    Mayo: Mayonnaise als Kondiment.
    Marmite: Marmite als Kondiment.
--}
data Kondiment = Senf | Mayo | Marmite
  deriving (Show, Eq)

{--
Eingaben:
    brot: Die Brotsorte des Sandwiches.

Erstellt ein leeres Sandwich mit der angegebenen Brotsorte und ohne Füllung, Beilagen oder Kondimente.
--}
sw :: Brot -> Sandwich
sw brot = Sandwich brot [] [] []

{--
Eingaben:
    s: Das aktuelle Sandwich.
    b: Die Füllung, die zum Sandwich hinzugefügt werden soll.

Fügt die angegebene Füllung zum Sandwich hinzu, falls die maximale Füllungsanzahl nicht überschritten wird.
--}
mit :: Sandwich -> Fuellung -> Sandwich
mit s b
  | not (check s) = s
  | otherwise = s {fuellung = b : fuellung s}

{--
Eingaben:
    s: Das aktuelle Sandwich.
    b: Die Beilage, die zum Sandwich hinzugefügt werden soll.

Fügt die angegebene Beilage zum Sandwich hinzu, falls die maximale Anzahl der Beilagen nicht überschritten wird.
--}
dazu :: Sandwich -> Beilage -> Sandwich
dazu s b
  | not (check s) = s
  | otherwise = s {beilagen = b : beilagen s}

{--
Eingaben:
    s: Das aktuelle Sandwich.
    k: Das Kondiment, das zum Sandwich hinzugefügt werden soll.

Fügt das Kondiment zum Sandwich hinzu, falls die maximale Anzahl und Kombination der Kondimente eingehalten wird.
--}
darauf :: Sandwich -> Kondiment -> Sandwich
darauf s k
  | isValid = s {kondimente = k : kondimente s}
  | otherwise = s
  where
    isValid = check s

{--
Eingaben:
    brot: Die Brotsorte des Sandwiches.

Erstellt ein "BLT"-Sandwich mit Bacon, Salat und Tomaten als Zutaten.
--}
blt :: Brot -> Sandwich
blt brot = Sandwich brot [Bacon] [Salat, Tomaten] []

{--
Eingaben:
    brot: Die Brotsorte des Sandwiches.

Erstellt ein "Ploughmans"-Sandwich mit Cheddar, Pickels und Salat.
--}
ploughmans :: Brot -> Sandwich
ploughmans brot = Sandwich brot [Cheddar] [Pickels, Salat] []

{--
Eingaben:
    brot: Die Brotsorte des Sandwiches.

Erstellt ein Gurkensandwich mit Gurken und zwei Mayo-Kondimenten.
--}
cucumber :: Brot -> Sandwich
cucumber brot = Sandwich brot [] [Gurken] [Mayo, Mayo]

{--
Eingaben:
    brot: Die Brotsorte des Sandwiches.

Erstellt ein Roastbeef-Sandwich mit Roastbeef, Salat und Senf.
--}
roastBeef :: Brot -> Sandwich
roastBeef brot = Sandwich brot [RoastBeef] [Salat] [Senf]

{--
Eingaben:
    brot: Die Brotsorte des Sandwiches.

Erstellt ein "Greek Delight"-Sandwich mit Feta, Salat und Oliven.
--}
greekDelight :: Brot -> Sandwich
greekDelight brot = Sandwich brot [Feta] [Salat, Oliven] []

{--
Eingaben:
    s: Das aktuelle Sandwich.

Überprüft, ob das Sandwich die maximal zulässige Anzahl an Zutaten und Kombinationen nicht überschreitet.
--}
check :: Sandwich -> Bool
check s
  | length (fuellung s) > 2 = False
  | length (beilagen s) > 5 = False
  | length (kondimente s) > 3 = False
  | (Marmite `elem` kondimente s) && (Mayo `elem` kondimente s) = False
  | otherwise = True

{--
Repräsentiert die verfügbaren Getränkesorten, die zu einer Bestellung hinzugefügt werden können.

Mögliche Werte:
    Cola: Cola als Getränk.
    Fanta: Fanta als Getränk.
    IrnBru: IrnBru als Getränk.
    Wasser: Wasser als Getränk.
--}
data Drink = Cola | Fanta | IrnBru | Wasser
  deriving (Show, Eq)

{--
Eingaben:
    sandwiches: Eine Liste von Sandwiches, die in der Bestellung enthalten sind.
    drinks: Eine Liste von Getränken, die in der Bestellung enthalten sind.

Beschreibung:
    Eine Bestellung enthält eine Sammlung von Sandwiches und Getränken, die der Kunde angefordert hat.
--}
data Bestellung = Bestellung {sandwiches :: [Sandwich], drinks :: [Drink]}
  deriving (Show, Eq)

{--
Eine leere Bestellung ohne Sandwiches und Getränke.

Beschreibung:
    `order` ist eine leere Bestellung, die als Ausgangspunkt für eine neue Bestellung dient.
--}
order :: Bestellung
order = Bestellung [] []

{--
Eingaben:
    b: Die aktuelle Bestellung.
    d: Das Getränk, das zur Bestellung hinzugefügt werden soll.

Fügt ein Getränk zur Bestellung hinzu.
--}
drink :: Bestellung -> Drink -> Bestellung
drink b d = b {drinks = d : drinks b}

{--
Eingaben:
    b: Die aktuelle Bestellung.
    s: Das Sandwich, das zur Bestellung hinzugefügt werden soll.

Fügt ein Sandwich zur Bestellung hinzu.
--}
sandwich :: Bestellung -> Sandwich -> Bestellung
sandwich b s = b {sandwiches = s : sandwiches b}

{--
Eingaben:
    s: Das Sandwich, in dem die Füllung gezählt wird.
    f: Die Füllung, die gezählt werden soll.

Gibt die Anzahl der Vorkommen der angegebenen Füllung im Sandwich zurück.
--}
cntFuellung :: Sandwich -> Fuellung -> Int
cntFuellung s f = length (filter (== f) (fuellung s))

{--
Eingabe:
    s: Ein Sandwich, für das die Anzahl der Füllungen ermittelt werden soll.

Berechnet die Anzahl der Füllungen in einem Sandwich.
    - Die Funktion ruft die `fuellung`-Funktion auf, um die Liste der Füllungen des Sandwichs zu erhalten.
    - `length` wird verwendet, um die Länge der Liste der Füllungen zu berechnen, was der Anzahl der Füllungen entspricht.
--}
cntFuellungen :: Sandwich -> Int
cntFuellungen s = length (fuellung s)

{--
Eingaben:
    s: Das Sandwich, in dem die Beilage gezählt wird.
    b: Die Beilage, die gezählt werden soll.

Gibt die Anzahl der Vorkommen der angegebenen Beilage im Sandwich zurück.
--}
cntBeilage :: Sandwich -> Beilage -> Int
cntBeilage s b = length (filter (== b) (beilagen s))

{--
Eingabe:
    s: Ein Sandwich, für das die Anzahl der Beilagen ermittelt werden soll.

Berechnet die Anzahl der Beilagen in einem Sandwich.
    - Die Funktion ruft die `beilagen`-Funktion auf, um die Liste der Beilagen des Sandwichs zu erhalten.
    - `length` wird verwendet, um die Länge der Liste der Beilagen zu berechnen, was der Anzahl der Beilagen entspricht.
--}
cntBeilagen :: Sandwich -> Int
cntBeilagen s = length (beilagen s)

{--
Eingaben:
    s: Das Sandwich, in dem das Kondiment gezählt wird.
    k: Das Kondiment, das gezählt werden soll.

Gibt die Anzahl der Vorkommen des angegebenen Kondiments im Sandwich zurück.
--}
cntKondiment :: Sandwich -> Kondiment -> Int
cntKondiment s k = length (filter (== k) (kondimente s))

{--
Eingabe:
    s: Ein Sandwich, für das die Anzahl der Kondimente ermittelt werden soll.

Berechnet die Anzahl der Kondimente in einem Sandwich.
    - Die Funktion ruft die `kondimente`-Funktion auf, um die Liste der Kondimente des Sandwichs zu erhalten.
    - `length` wird verwendet, um die Länge der Liste der Kondimente zu berechnen, was der Anzahl der Kondimente entspricht.
--}
cntKondimente :: Sandwich -> Int
cntKondimente s = length (kondimente s)

{--
Eingaben:
    s: Das Sandwich, dessen Preis berechnet wird.

Berechnet den Preis des Sandwiches, basierend auf den Zutaten und deren Kombination.
--}
preisSandwich :: Sandwich -> Int
preisSandwich s
  | blt = 245
  | plghs = 240
  | cucmbr = 95
  | roastb = 250
  | greekd = 260
  | kleinesS = 275
  | grossesS = 375
  | otherwise =
      brotPreis (brot s)
        + sum (map fuellungPreis (fuellung s))
        + sum (map beilagePreis (beilagen s))
        + sum (map kondimentPreis (kondimente s))
  where

    blt = (cntFuellung s Bacon == 1 && cntFuellungen s == 1) && (cntBeilage s Salat == 1 && cntBeilage s Tomaten == 1 && cntBeilagen s == 2) && (cntKondimente s == 0)
    plghs = (cntFuellung s Cheddar == 1 && cntFuellungen s == 1) && (cntBeilage s Pickels == 1 && cntBeilage s Salat == 1 && cntBeilagen s == 2) && (cntKondimente s == 0)
    cucmbr = (cntFuellungen s == 0) && (cntBeilage s Gurken == 1 && cntBeilagen s == 1) && (cntKondiment s Mayo == 2 && cntKondimente s == 2)
    roastb = (cntFuellung s RoastBeef == 1 && cntFuellungen s == 1) && (cntKondiment s Senf == 1 && cntKondimente s == 1) && (cntBeilage s Salat == 1 && cntBeilagen s == 1)
    greekd = (cntFuellung s Feta == 1 && cntFuellungen s == 1) && (cntBeilage s Salat == 1 && cntBeilage s Oliven == 1 && cntBeilagen s == 2) && (cntKondimente s == 0)
    kleinesS= cntFuellungen s == 1 && cntBeilagen s == 2 && cntKondimente s == 1 
    grossesS = cntFuellungen s == 2 && cntBeilagen s == 3 && cntKondimente s == 2

    brotPreis Toastbrot = 50
    brotPreis Roggenbrot = 70

    fuellungPreis Bacon = 160
    fuellungPreis Feta = 160
    fuellungPreis RoastBeef = 160
    fuellungPreis Cheddar = 130
    fuellungPreis Salami = 130

    beilagePreis Pickels = 75
    beilagePreis Salat = 60
    beilagePreis Tomaten = 60
    beilagePreis Gurken = 60
    beilagePreis Oliven = 60

    kondimentPreis Senf = 10
    kondimentPreis Mayo = 10
    kondimentPreis Marmite = 10

{--
Eingaben:
    Ein Getränk (`Drink`), dessen Preis ermittelt werden soll.

Gibt den Preis des angegebenen Getränks zurück.
--}
preisDrink :: Drink -> Int
preisDrink Cola = 100
preisDrink Wasser = 80
preisDrink Fanta = 100
preisDrink IrnBru = 100

{--
Eingaben:
    b: Die Bestellung, deren Preis berechnet wird.

Berechnet den Gesamtpreis der Bestellung, einschließlich Sandwiches und Getränke.
--}
preis :: Bestellung -> Int
preis b = sum (map preisSandwich (sandwiches b)) + sum (map preisDrink (drinks b))

{--
Eingaben:
    Start: Ein Startpunkt für die Liste von Bestellungen.
    BestellungList: Eine Bestellung, gefolgt von weiteren Bestellungen in der Liste.

Beschreibung:
    Diese Datenstruktur repräsentiert eine Sammlung von Bestellungen, die rekursiv verknüpft sind.
--}
data Bestellungen = Start | BestellungList Bestellung Bestellungen
  deriving (Show, Eq)

{--
Eingaben:
    start: Der Startwert für die Bestellungen.

Gibt eine leere Bestellliste zurück.
--}
start :: Bestellungen
start = Start

{--
Eingaben:
    - `Bestellungen`: Eine Liste von bestehenden Bestellungen.
    - `neueBestellung`: Eine neue Bestellung, die der Liste hinzugefügt werden soll.

Fügt eine neue Bestellung (`neueBestellung`) zur bestehenden Liste der Bestellungen (`Bestellungen`) hinzu.
    - Wenn die bestehende Liste `Start` (leer) ist, wird eine neue Liste mit der `neueBestellung` und `Start` als Endpunkt erstellt.
    - Andernfalls wird die `neueBestellung` an den Anfang der bestehenden Liste angefügt.
--}
bestellung :: Bestellungen -> Bestellung -> Bestellungen
bestellung Start neueBestellung = BestellungList neueBestellung Start
bestellung (BestellungList bestellung rest) neueBestellung = BestellungList neueBestellung (BestellungList bestellung rest)

{--
Eingaben:
    - `Bestellungen`: Eine Liste von Bestellungen.
    - `Fuellung`: Eine bestimmte Füllung, deren Vorkommen gezählt werden soll.

Berechnet die Gesamtanzahl, wie oft eine bestimmte `Fuellung` in allen Sandwiches einer Liste von Bestellungen vorkommt.
    - Wenn `Bestellungen` leer (`Start`) ist, wird 0 zurückgegeben.
    - Andernfalls wird in der ersten Bestellung nach Sandwiches gesucht, die die gewünschte `Fuellung` enthalten, und deren Anzahl zur Summe addiert.
    - Die Funktion ruft sich rekursiv auf, um die restlichen Bestellungen zu verarbeiten.
--}
wieviel :: Bestellungen -> Fuellung -> Int
wieviel Start _ = 0
wieviel (BestellungList (Bestellung sandwiches _) rest) filling =
  length (filter (elem filling . sandwichFuellungen) sandwiches) + wieviel rest filling
  where
    sandwichFuellungen (Sandwich _ fillings _ _) = fillings

{--
Eingaben:
    - `Bestellungen`: Eine Liste von Bestellungen, die jeweils eine Liste von Sandwiches und Getränken enthält.

Berechnet den Gesamtumsatz aller Bestellungen in der Liste.
    - Wenn `Bestellungen` leer (`Start`) ist, wird 0 zurückgegeben.
    - Andernfalls wird der Preis aller Sandwiches und Getränke in der ersten Bestellung summiert.
    - Die Funktion ruft sich rekursiv auf, um die Umsätze der restlichen Bestellungen zu addieren.
--}
umsatz :: Bestellungen -> Int
umsatz Start = 0
umsatz (BestellungList (Bestellung sandwiches drinks) rest) =
  sum (map preisSandwich sandwiches) + sum (map preisDrink drinks) + umsatz rest
