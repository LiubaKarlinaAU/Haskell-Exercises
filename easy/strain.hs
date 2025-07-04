-- Implement the keep and discard operation on collections.
-- https://exercism.org/tracks/haskell/exercises/strain

module Strain (keep, discard) where
discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p (x:xs) = if p x then rest else x : rest
                   where rest = discard p xs
keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs) = if p x then x : rest else rest
                   where rest = keep p xs