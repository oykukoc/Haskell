module TestData where

import Data.Array

arr0,arr1,arr2,arr3,arr4,arr5,arr6,arr7,arr8,arr9 :: Array Integer String

arr0 = array (7,5) []

arr1 = array (0,3) [(0,"hasukuro"),(1,"hasukuro"),(2,"nala"),(3,"tom")]
arr2 = array (1,4) [(1,"nala"),(2,"hasukuro"),(3,"tom"),(4,"hasukuro")]

arr3 = array (2,5) [(2,"nala"), (3,"tom"), (4,"hasukuru"), (5,"rocket")]
arr4 = array (2,5) [(2,"hasukuru"), (3,"nala"), (4,"rocket"), (5,"tom")]
arr5 = array (2,5) [(2,"nala"), (3,"hasukuru"), (4,"tom"), (5,"rocket")]
arr6 = array (2,5) [(2,"hasukuru"), (3,"nala"), (4,"tom"), (5,"rocket")]

arr7 = array (2,3) [(2,"nala"), (3,"tom")]
arr8 = array (4,5) [(4,"hasukuru"), (5,"rocket")]
arr9 = array (2,4) [(2,"hasukuru"), (3,"nala"), (4,"tom")]
