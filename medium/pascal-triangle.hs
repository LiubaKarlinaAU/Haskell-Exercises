-- Compute Pascal's triangle up to a given number of rows.
-- https://exercism.org/tracks/haskell/exercises/pascals-triangle

module Triangle (rows) where
rows :: Int -> [[Integer]]
rows x 
     | x <= 0 = []
     | x == 1 = [[1]] 
     | otherwise = [1] : rows' [1] (x - 1)
rows' :: [Integer] -> Int -> [[Integer]]
rows' prev x 
      | x == 0 = []
      | otherwise = curRow : rows' curRow (x - 1)
      where curRow = foldr (\x acc -> x : (head acc + x) : tail acc) [0] prev