-- Given a DNA strand, return its RNA Complement Transcription.
-- https://exercism.org/tracks/haskell/exercises/rna-transcription

module DNA (toRNA) where
import qualified Data.List as L
import qualified Data.Maybe as M
toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA xs = case wrongNuc of
             Just a -> Left a
             Nothing -> Right . map matching $ xs
      where 
        dna_nucl =  ['G', 'C', 'T', 'A']
        rna_nucl = ['C', 'G', 'A', 'U']
        wrongNuc = L.find (\x -> not (elem x dna_nucl)) xs
        matching x = rna_nucl !! M.fromJust (L.elemIndex x dna_nucl) 