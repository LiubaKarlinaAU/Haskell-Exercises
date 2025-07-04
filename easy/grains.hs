-- Calculate the number of grains of wheat on a chessboard given that the number on each square doubles.
-- https://exercism.org/tracks/haskell/exercises/grains

module Grains (square, total) where
square :: Integer -> Maybe Integer
square n 
      | n `elem` [1..64] = Just $ iterate (*2) 1 !! fromIntegral (n-1)
      | otherwise = Nothing
total :: Integer
total = sum . (take 64) $ iterate (*2) 1