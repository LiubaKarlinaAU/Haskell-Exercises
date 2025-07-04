-- Given a DNA string, compute how many times each nucleotide occurs in the string.
-- https://exercism.org/tracks/haskell/exercises/nucleotide-count

module DNA (nucleotideCounts, Nucleotide(..)) where
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sort, groupBy)
data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts [] = Right M.empty
nucleotideCounts xs = if any (== err) converted then err 
                      else
                         Right . addToMap . map getRight $ converted 
                 where
                   converted = map conversion xs
                   err = Left "error" 
                   getRight x = case x of 
                            Right x -> x
                            _ -> A -- never happens
                            
conversion :: Char -> Either String Nucleotide
conversion c = case c of
               'A' -> Right A  
               'C' -> Right C 
               'G' -> Right G 
               'T' -> Right T 
               _ -> Left "error"
addToMap :: [Nucleotide] -> Map Nucleotide Int
addToMap =  M.fromListWith (+) . map (\x -> (x,1))