-- Add the numbers to a minesweeper board.
-- https://exercism.org/tracks/haskell/exercises/minesweeper

module Minesweeper (annotate) where

import qualified Data.Char as C

data Cell = Empty | Mine deriving Eq

annotate :: [String] -> [String]
annotate matrix =  map (map transformCell) $ zipWith zip matrix (indexMatrix width height)
         where  
                  height = length matrix
                  width = length (matrix !! 0)
                  getCellValue :: Int -> Int -> Cell
                  getCellValue x y = if x < 0 || x >= height || y < 0 || y >= width then Empty 
                                     else let c = matrix !! x !! y 
                                          in if c == '*' then Mine else Empty
                  getMinesNumber :: Int -> Int -> Int
                  getMinesNumber x = length . filter (== Mine) . map (\(a,b) -> getCellValue a b) . (getNeighbors x)
                  transformCell :: (Char, (Int,Int)) -> Char
                  transformCell (c, (x, y)) = if c == '*' then '*' else 
                                                 let 
                                                   minesN = getMinesNumber x y 
                                                 in 
                                                   if minesN == 0 then ' ' else C.chr (C.ord '0' + minesN) 


indexMatrix :: Int -> Int -> [[(Int,Int)]]
indexMatrix w h = map (\x -> map (\y -> (x,y)) [0..(w - 1)]) [0..(h - 1)] 

getNeighbors :: Int -> Int -> [(Int,Int)]
getNeighbors x y = [(x - 1, y - 1), 
                    (x, y - 1), 
                    (x + 1, y - 1), 
                    (x - 1, y),
                    (x + 1, y),
                    (x - 1, y + 1),
                    (x, y + 1),
                    (x + 1, y + 1)]