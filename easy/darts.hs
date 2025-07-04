-- Write a function that returns the earned points in a single toss of a Darts game.
-- https://exercism.org/tracks/haskell/exercises/darts

module Darts (score) where
score :: Float -> Float -> Int
score x y 
      | dist > 10 = 0
      | dist > 5 = 1
      | dist > 1 = 5
      | otherwise = 10
      where dist = sqrt (x * x + y * y)