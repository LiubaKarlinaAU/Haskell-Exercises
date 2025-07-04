-- Given a number, find the sum of all the multiples of particular numbers up to but not including the number.
-- https://exercism.org/tracks/haskell/exercises/sum-of-multiples

module SumOfMultiples (sumOfMultiples) where
import qualified Data.List as L
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . L.nub . concat . map (\x -> [x, (2 * x) .. (limit-1)]) $  filter (/= 0) factors 