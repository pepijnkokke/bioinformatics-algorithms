{-# LANGUAGE TupleSections #-}

import NucleicAcid

import Control.Arrow
import Data.List (elemIndex,isInfixOf)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe,listToMaybe)
import Data.String.Utils (strip)
import Data.Tuple (swap)
import System.IO (getLine,readLn)



-- * Protein Translation Problem

-- |Translate an RNA string into an amino acid string.
--  Input: An RNA string Pattern.
--  Output: The translation of Pattern into an amino acid string Peptide.
--  Notes:
--  * The "Stop" codon should not be translated, as shown in the sample below.
--  * For your convenience, we provide a downloadable RNA codon table indicating
--    which codons encode which amino acids.
problem1 :: IO ()
problem1 = putStrLn . decodeRna =<< getLine

-- |Convert an RNA string to the corresponding peptide.
decodeRna :: RNA -> Peptide
decodeRna = mapMaybe decodeCodon . chunksOf 3

-- |Convert a codon to the corresponding amino acid.
decodeCodon :: Codon -> Maybe AminoAcid
decodeCodon = flip M.lookup codonToAminoAcid

-- |Table mapping codons to their respective amino acid.
codonToAminoAcid :: Map Codon AminoAcid
codonToAminoAcid = M.fromList encodingPairs

-- |List of codons paired with their respective amino acids.
encodingPairs :: [(Codon,AminoAcid)]
encodingPairs =
  [("AAA",'K'),("AAC",'N'),("AAG",'K'),("AAU",'N'),("ACA",'T'),("ACC",'T'),("ACG",'T')
  ,("ACU",'T'),("AGA",'R'),("AGC",'S'),("AGG",'R'),("AGU",'S'),("AUA",'I'),("AUC",'I')
  ,("AUG",'M'),("AUU",'I'),("CAA",'Q'),("CAC",'H'),("CAG",'Q'),("CAU",'H'),("CCA",'P')
  ,("CCC",'P'),("CCG",'P'),("CCU",'P'),("CGA",'R'),("CGC",'R'),("CGG",'R'),("CGU",'R')
  ,("CUA",'L'),("CUC",'L'),("CUG",'L'),("CUU",'L'),("GAA",'E'),("GAC",'D'),("GAG",'E')
  ,("GAU",'D'),("GCA",'A'),("GCC",'A'),("GCG",'A'),("GCU",'A'),("GGA",'G'),("GGC",'G')
  ,("GGG",'G'),("GGU",'G'),("GUA",'V'),("GUC",'V'),("GUG",'V'),("GUU",'V'),("UAC",'Y')
  ,("UAU",'Y'),("UCA",'S'),("UCC",'S'),("UCG",'S'),("UCU",'S'),("UGC",'C'),("UGG",'W')
  ,("UGU",'C'),("UUA",'L'),("UUC",'F'),("UUG",'L'),("UUU",'F')]



-- * Peptide Encoding Problem

-- |Find substrings of a genome encoding a given amino acid sequence.
--  Input: A DNA string Text and an amino acid string Peptide.
--  Output: All substrings of Text encoding Peptide (if any such substrings exist).
problem2 :: IO ()
problem2 = do
  dna <- getLine
  pep <- getLine

  putStrLn . unlines
    . filter (`isInfixOf` dna)
    . withReverseComplements
    . map transcribeRnaString
    . encodePeptide $ pep

-- |Convert a peptide to the corresponding RNA strings.
encodePeptide :: Peptide -> [RNA]
encodePeptide = map concat . foldr (\a rs -> [ c : cs | c <- encodeAminoAcid a, cs <- rs ]) [[]]

-- |Convert an amino acid to the corresponding codons.
encodeAminoAcid :: AminoAcid -> [Codon]
encodeAminoAcid a = M.findWithDefault [] a aminoAcidToCodons

-- |Table mapping amino acids to their respective codons.
aminoAcidToCodons :: Map AminoAcid [Codon]
aminoAcidToCodons = M.fromListWith (++) [ (a,[c]) | (c,a) <- encodingPairs ]

-- |Add reverse complements to a list of DNA strings.
withReverseComplements :: [DNA] -> [DNA]
withReverseComplements gs = gs ++ map (reverse . complementDnaString) gs

main = problem2
