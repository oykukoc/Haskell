module Sum where

summe :: Integer-> Integer
summe n = if n == 0 then 0 else n + summe( n - 1 )

