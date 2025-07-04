-- Implement Conway's Game of Life
-- https://exercism.org/tracks/haskell/exercises/game-of-life

module GameOfLife (tick) where
tick :: [[Int]] -> [[Int]]
tick matrix = map (map transformCell) $ zipWith zip matrix (indexMatrix width height)
            where 
                  width = length matrix
                  height = length (matrix !! 0)
                  getCellValue :: Int -> Int -> Int
                  getCellValue x y = if x < 0 || x >= height || y < 0 || y >= width
                                     then 0 
                                     else matrix !! x !! y
                  getNeighbors :: Int -> Int -> [(Int,Int)]
                  getNeighbors x y = [(x - 1, y - 1), 
                                      (x, y - 1), 
                                      (x + 1, y - 1), 
                                      (x - 1, y),
                                      (x + 1, y),
                                      (x - 1, y + 1),
                                      (x, y + 1),
                                      (x + 1, y + 1)]
                  isAlive :: Int -> Int -> Bool 
                  isAlive x y = getCellValue x y == 1
                  getAliveNeighborsNumber :: Int -> Int -> Int
                  getAliveNeighborsNumber x = sum . map (\(a,b) -> getCellValue a b) . (getNeighbors x)
                  transformCell :: (Int, (Int,Int)) -> Int
                  transformCell (c, (x, y)) = let 
                                                aliveN = getAliveNeighborsNumber x y 
                                             in 
                                                if isAlive x y 
                                                   then (if aliveN == 2 || aliveN == 3 
                                                         then 1 
                                                         else 0) 
                                                   else if aliveN == 3 
                                                           then 1 
                                                           else 0 
indexMatrix :: Int -> Int -> [[(Int,Int)]]
indexMatrix w h = map (\x -> map (\y -> (x,y)) [0..(w - 1)]) [0..(h - 1)]     