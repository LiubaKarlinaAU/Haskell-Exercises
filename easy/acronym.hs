-- Convert a phrase to its acronym.
-- https://exercism.org/tracks/haskell/exercises/acronym

module Acronym (abbreviate) where
abbreviate :: String -> String
abbreviate xs = abb (' ': cleanTheInput xs) False False
cleanTheInput :: String -> String
cleanTheInput [] = []
cleanTheInput (x:xs)
            | isLetter x || x == ' ' || x == '-' = x : cleanTheInput xs
            | otherwise = cleanTheInput xs
            
abb :: String -> Bool -> Bool -> String
abb [] _ _ = []
abb (x:xs) isWordStarts isPreviousSmall
   | x == ' ' || x == '-' = abb xs True False
   | isWordStarts = toUpper x : abb xs False False
   | isUpper x && isPreviousSmall = x : abb xs False False
   | isSmaller x = abb xs False True
   | otherwise = abb xs False False
isLetter :: Char -> Bool
isLetter c = elem c ['a'..'z'] || elem c ['A'..'Z']
isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'
isSmaller :: Char -> Bool
isSmaller c = 'a' <= c && c <= 'z'
toUpper :: Char -> Char
toUpper c = let result = lookup c (zip ['a'..'z'] ['A'..'Z'])
            in case result of
                    Just a -> a
                    Nothing -> c