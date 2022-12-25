module Main where

import Data.Validation
import Data.List.NonEmpty
import Control.Arrow (left)
import Flow

romanChar2Int :: Char -> Either String Int
romanChar2Int 'I' = Right 1
romanChar2Int 'V' = Right 5
romanChar2Int 'X' = Right 10
romanChar2Int 'L' = Right 50
romanChar2Int 'C' = Right 100
romanChar2Int 'D' = Right 500
romanChar2Int 'M' = Right 1000
romanChar2Int e = Left ('\'' : e : "' is not a valid Roman character")

either2Validated :: Either a b -> Validation (NonEmpty a) b
either2Validated e = fromEither (left singleton e)

roman2IntValidated :: (NonEmpty Char) -> Validation (NonEmpty String) (NonEmpty Int)
roman2IntValidated l = traverse (romanChar2Int .> either2Validated) l

main = do
  print(romanChar2Int 'C')
  print(romanChar2Int 'J')
  print(roman2IntValidated (fromList "ABC"))
  print(roman2IntValidated (fromList "MMXXII"))
