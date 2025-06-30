{-# LANGUAGE InstanceSigs #-}

module Data where

import Bits
import Data.Word (Word8)

class Encodeable a where
  encodeToBit :: a -> BitSeq
  decodeFromBit :: BitSeq -> a
  bitSize :: a -> Int

instance Encodeable Bool where
  {--
Eingaben:
    b: Ein Bool-Wert, der in ein BitSeq kodiert werden soll.

Berechnung:
    - Wenn `b` True ist, wird `I E` (1) zurückgegeben.
    - Wenn `b` False ist, wird `O E` (0) zurückgegeben.
--}
  encodeToBit :: Bool -> BitSeq
  encodeToBit True = I E
  encodeToBit False = O E

{--
Eingaben:
    bs: Ein BitSeq, das dekodiert werden soll (d.h. eine bitweise Darstellung eines Bool-Werts).

Berechnung:
    - Die Funktion rekursiv dekodiert die Bits des BitSeq.
    - Wenn das BitSeq mit `I E` endet, wird `True` zurückgegeben.
    - Wenn das BitSeq mit `O E` endet, wird `False` zurückgegeben.
    - Ansonsten wird rekursiv das nächste Bit verarbeitet, bis das BitSeq vollständig dekodiert ist.
--}
  decodeFromBit :: BitSeq -> Bool
  decodeFromBit bs = case bs of
    I E -> True
    O E -> False
    I rest -> decodeFromBit rest
    O rest -> decodeFromBit rest
    E -> False

{--
Eingaben:
    b: Ein `Bool`, für das die Bitgröße berechnet werden soll.

Berechnung:
    - Wenn `b` gleich `True` ist, wird `1` zurückgegeben.
    - Wenn `b` gleich `False` ist, wird `0` zurückgegeben.
--}
  bitSize :: Bool -> Int
  bitSize True = 1
  bitSize False = 0


instance Encodeable Word8 where
{--
Eingaben:
    n: Eine Zahl (Word8), die in ein BitSeq codiert werden soll.

Berechnung:
    - Die Zahl wird rekursiv durch Division mit 2 zerlegt.
    - Für jede Division wird ein Bit (entweder `I` für 1 oder `O` für 0) dem BitSeq hinzugefügt.
    - Die rekursive Funktion stellt sicher, dass die Bits in der richtigen Reihenfolge (von MSB zu LSB) kodiert werden.
    - Die Funktion `toNumericLength` sorgt dafür, dass das resultierende BitSeq die richtige Länge von 8 Bits hat.
--}
  encodeToBit :: Word8 -> BitSeq
  encodeToBit 0 = E
  encodeToBit n
    | n `mod` 2 == 1 = toNumericLength (encodeToBit (n `div` 2) `appendBit` I E) 8
    | otherwise = toNumericLength (encodeToBit (n `div` 2) `appendBit` O E) 8
    where
      appendBit :: BitSeq -> BitSeq -> BitSeq
      appendBit E bit = bit
      appendBit (I rest) bit = I (rest `appendBit` bit)
      appendBit (O rest) bit = O (rest `appendBit` bit)

{--
Eingaben:
    bs: Ein BitSeq, das als binäre Zahl interpretiert und in einen Word8-Wert umgewandelt werden soll.

Berechnung:
    - Wenn der erste Bit `I` (1) ist, wird `2 ^ bitLength rest` zum Ergebnis addiert und die Funktion rekursiv auf den Rest angewendet.
    - Wenn der erste Bit `O` (0) ist, wird nur die Funktion rekursiv auf den Rest angewendet.
    - Wenn das Ende erreicht ist (`E`), wird 0 zurückgegeben.
--}
  decodeFromBit :: BitSeq -> Word8
  decodeFromBit bs = case bs of
    I rest -> (2 ^ bitLength rest) + decodeFromBit rest
    O rest -> decodeFromBit rest
    E -> 0

{--
Eingaben:
    _ : Ein beliebiges `Word8`-Wert (0 bis 255).

Berechnung:
    - Die Funktion gibt immer 8 zurück, da `Word8` standardmäßig 8 Bits hat.
--}
  bitSize :: Word8 -> Int
  bitSize _ = 8

{--
Eingaben:
    xs: Eine Liste von Elementen vom Typ `Encodeable`, die in eine `BitSeq` umgewandelt werden sollen.

Berechnung:
    - Jedes Element in der Liste wird mit der Funktion `encodeToBit` in eine `BitSeq` umgewandelt.
    - Dann werden die Bit-Sequenzen der Elemente der Liste mit der Funktion `appendBit` zu einer langen `BitSeq` kombiniert.
--}
encodeList :: (Encodeable a) => [a] -> BitSeq
encodeList [] = E
encodeList (x : xs) = encodeToBit x `appendBit` encodeList xs
  where
    appendBit :: BitSeq -> BitSeq -> BitSeq
    appendBit E bit = bit
    appendBit (I rest) bit = I (rest `appendBit` bit)
    appendBit (O rest) bit = O (rest `appendBit` bit)

{--
Eingabe:
    dummy: Ein Beispielwert des Typs (`Encodeable`), der zur Bestimmung der Dekodiergröße des Bit-Sequenzes dient.
    bits: Die zu dekodierende `BitSeq` (Bit-Sequenz).

Beschreibung:
    Die Funktion `decodeList` wandelt eine Bit-Sequenz (`BitSeq`) in eine Liste von Elementen um, 
    die zur Klasse `Encodeable` gehören. In jedem Schritt wird die Sequenz in zwei Teile zerlegt:
    1. Der erste Teil (`current`) wird dekodiert und zur Ergebnisliste hinzugefügt.
    2. Der verbleibende Teil (`rest`) wird rekursiv weiterverarbeitet.

    Wenn die Eingabe-Bit-Sequenz leer (`E`) ist, wird eine leere Liste zurückgegeben.

Hilfsfunktion (`splitBits`):
    Diese Hilfsfunktion teilt die Bit-Sequenz in zwei Teile, basierend auf der angegebenen Länge:
    - Der erste Teil (`BitSeq`) hat die angegebene Länge.
    - Der zweite Teil enthält die verbleibenden Bits.
--}
decodeList :: (Encodeable a) => a -> BitSeq -> [a]
decodeList _ E = []
decodeList dummy bits =
  case splitBits bits (bitSize dummy) of
    (current, rest) -> decodeFromBit current : decodeList dummy rest
  where
    splitBits :: BitSeq -> Int -> (BitSeq, BitSeq)
    splitBits E _ = (E, E)
    splitBits b 0 = (E, b)
    splitBits (I rest) n =
      let (first, second) = splitBits rest (n - 1)
      in (I first, second)
    splitBits (O rest) n =
      let (first, second) = splitBits rest (n - 1)
      in (O first, second)