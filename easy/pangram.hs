-- Determine if a sentence is a pangram.
-- https://exercism.org/tracks/haskell/exercises/pangram

module Pangram (isPangram) where
import qualified Data.List as L
import qualified Data.Char as C
isPangram :: String -> Bool
isPangram text = (length . L.nub . filter ( `elem` ['a'..'z']) . map C.toLower ) text == 26 
