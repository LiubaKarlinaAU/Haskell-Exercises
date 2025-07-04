-- Determine if a number is perfect, abundant, or deficient based on Nicomachus' (60 - 120 CE) classification scheme for positive integers.
-- https://exercism.org/tracks/haskell/exercises/perfect-numbers

module PerfectNumbers (classify, Classification(..)) where
data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)
classify :: Int -> Maybe Classification
classify n
       | n <= 0 = Nothing
       | s == n = Just Perfect
       | s < n = Just Deficient
       | s > n = Just Abundant
       where divid x y = mod x y == 0
             limit = n - 1
             s = sum . filter (divid n) $ [1..limit]