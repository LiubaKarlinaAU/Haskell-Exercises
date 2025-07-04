-- Given a number determine whether or not it is valid per the Luhn formula.
-- https://exercism.org/tracks/haskell/exercises/luhn

module Luhn (isValid) where
import Data.Char as C
import Data.List as L
isValid :: String -> Bool
isValid [] = False
isValid (x:[]) = False
isValid xs = hasOnlyDigit && isLong && mod luchSum 10 == 0 
             where cleaned = filter (not . C.isSpace) xs
                   isLong = length cleaned > 1
                   hasOnlyDigit = L.all (`elem` ['0'..'9']) cleaned
                   luchSum = countTheSum . reverse . map C.digitToInt $ cleaned
countTheSum :: [Int] -> Int
countTheSum [] = 0
countTheSum [x] = x
countTheSum (x:y:xs) = x + newY + countTheSum xs 
            where doubleY = y * 2
                  newY = if doubleY > 9 then doubleY - 9 else doubleY