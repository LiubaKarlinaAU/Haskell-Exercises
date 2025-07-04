-- Given a number n, determine what the nth prime is.
-- https://exercism.org/tracks/haskell/exercises/nth-prime

module Prime (nth) where
nth :: Int -> Maybe Integer
nth n 
    | n <= 0 = Nothing
    | otherwise = Just $ helper [] n 1
helper :: [Integer] -> Int -> Integer -> Integer
helper xs 0 l = l
helper xs n l = helper (newX:xs) (n-1) newX
                where find p = head . filter p
                      newX = find (\x -> not (any (\y -> x `mod` y == 0) xs )) $ [(l + 1)..]