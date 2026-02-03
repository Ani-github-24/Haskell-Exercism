module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map.Strict as M
import Control.Monad (foldM)

data Nucleotide = A | C | G | T
    deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (M.Map Nucleotide Int)
nucleotideCounts xs = foldM countNucleotide initialMap xs
  where
    
    initialMap = M.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    
    countNucleotide m char = do
        n <- toNucleotide char
        return $ M.adjust (+ 1) n m

toNucleotide :: Char -> Either String Nucleotide
toNucleotide 'A' = Right A
toNucleotide 'C' = Right C
toNucleotide 'G' = Right G
toNucleotide 'T' = Right T
toNucleotide x   = Left ("Invalid nucleotide: " ++ [x])