-- Calculate the number of steps to reach 1 using the Collatz conjecture.
-- https://exercism.org/tracks/haskell/exercises/collatz-conjecture

module CollatzConjecture (collatz) where
collatz :: Integer -> Maybe Integer
collatz n 
       | n <= 0 = Nothing 
       | otherwise = Just . toInteger . length . takeWhile (/= 1) . collatzRec $ n
collatzRec :: Integer -> [Integer]
collatzRec 1 = [1]
collatzRec n 
          | mod n 2 == 0 = n : collatzRec (div n 2)
          | otherwise = n : collatzRec (3 * n + 1)