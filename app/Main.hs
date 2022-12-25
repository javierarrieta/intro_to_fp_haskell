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

fromListSafe :: [a] -> Maybe (NonEmpty a)
fromListSafe [] = Nothing
fromListSafe (head : tail) = Just (head :| tail)

roman2MaybeInts :: String -> Maybe (Validation (NonEmpty String) (NonEmpty Int))
roman2MaybeInts a = fmap roman2IntValidated (fromListSafe a)

-- roman2Ints :: String -> Validation (NonEmpty String) (NonEmpty Int)

main = do
  print(romanChar2Int 'C')
  print(romanChar2Int 'J')
  print(roman2MaybeInts "ABC")
  print(roman2MaybeInts "MMXXII")
  print(roman2MaybeInts "")
