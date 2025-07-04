-- Given a word and a list of possible anagrams, select the correct sublist.
-- https://exercism.org/tracks/haskell/exercises/anagram

module Anagram (anagramsFor) where
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Function as F
import qualified Data.Char as C
anagramsFor :: String -> [String] -> [String]
anagramsFor [] _ = []
anagramsFor _ [] = []
anagramsFor word ys = filter (\x -> lower x /= lowerWord) . filter (\x -> unify x == unifiedWord) $ ys  
                       where 
                         lower = map C.toLower
                         unify = L.sort . lower
                         lowerWord = lower word
                         unifiedWord = unify word 