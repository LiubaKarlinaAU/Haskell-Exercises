-- Given a phrase, count the occurences of each word in that phrase.
-- https://exercism.org/tracks/haskell/exercises/word-count

module WordCount (wordCount) where
import qualified Data.Char as C
import qualified Data.List as L

wordCount :: String -> [(String, Int)]
wordCount xs =  map (\l -> (head l, length l)) . L.groupBy (==) . L.sort . map (\w -> if head w == '\'' then tail w else w) . map (\w -> if last w == '\'' then init w else w) . words $ cleaned
          where cleaned = map (\x -> if x `elem` approved then C.toLower x else ' ') xs
                approved = '\'' : ['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'9']
