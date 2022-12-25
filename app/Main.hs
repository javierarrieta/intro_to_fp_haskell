module Main where

import Data.Validation
import Data.List.NonEmpty

romanChar2Int :: Char -> Validation [String] Int
romanChar2Int 'I' = Success 1
romanChar2Int 'V' = Success 5
romanChar2Int 'X' = Success 10
romanChar2Int 'L' = Success 50
romanChar2Int 'C' = Success 100
romanChar2Int 'D' = Success 500
romanChar2Int 'M' = Success 1000
romanChar2Int e = Failure [('\'' : e : "' is not a valid Roman character")]

roman2IntValidated :: [Char] -> Validation [String] [Int]
roman2IntValidated l = traverse romanChar2Int l

main = do
  print (romanChar2Int 'C')
  print (romanChar2Int 'J')
  print(roman2IntValidated "ABC")
  print(roman2IntValidated "MMXXII")
