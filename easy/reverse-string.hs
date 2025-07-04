-- Reverse a given string. 
-- https://exercism.org/tracks/haskell/exercises/reverse-string

module ReverseString (reverseString) where
reverseString :: String -> String
reverseString "" = ""
reverseString xs = last xs : reverseString (init xs)