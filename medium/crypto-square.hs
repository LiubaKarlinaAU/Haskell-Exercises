-- Implement the classic method for composing secret messages called a square code.
-- https://exercism.org/tracks/haskell/exercises/crypto-square

module CryptoSquare (encode) where
import qualified Data.Char as C
import qualified Data.List as L
encode :: String -> String
encode xs = L.intercalate " " . map getCharColumn $ columns
            where cleaned = filter notSpaceOrSign . map C.toLower $ xs
                  notSpaceOrSign x = (not . C.isSpace $ x) && (not (x `elem` punctuations)) 
                  len = length cleaned
                  c = ceiling . sqrt . fromIntegral $ len
                  r = if c * (c-1) >= len then c - 1 else c
                  steps = [0,c..c * (r-1)]
                  columns = [0..c-1]
                  punctuations = [',','.','!','?',':','-','@','%']
                  getCharColumn x = map getCharOrSpace . map (x+) $ steps
                  getCharOrSpace ind = if ind < len then cleaned !! ind else ' ' 