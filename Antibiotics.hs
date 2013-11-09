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
problem1 = putStrLn . encoding =<< getLine

-- |Convert an RNA string to the corresponding peptide.
encoding :: RNA -> Peptide
encoding = mapMaybe codon2amino . chunksOf 3

-- |Convert a codon to the corresponding amino acid.
codon2amino :: Codon -> Maybe AminoAcid
codon2amino = flip M.lookup tbl
  where tbl = M.fromList dat

dat :: [(Codon,AminoAcid)]
dat =
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
    . map rna2dna
    . decodings $ pep

-- |Convert a DNA string to an RNA string.
dna2rna :: DNA -> RNA
dna2rna = map conv
  where conv 'T' = 'U'
        conv  n  =  n

-- |Convert a RNA string to an DNA string.
rna2dna :: RNA -> DNA
rna2dna = map conv
  where conv 'U' = 'T'
        conv  n  =  n

decodings :: Peptide -> [RNA]
decodings = map concat . foldr (\a rs -> [ c : cs | c <- amino2codons a, cs <- rs ]) [[]]

amino2codons :: AminoAcid -> [Codon]
amino2codons a = M.findWithDefault [] a tbl
  where
    tbl = M.fromListWith (++) [ (a,[c]) | (c,a) <- dat ]

{-
pep2rna :: Map AminoAcid [Codon] -> Peptide -> [RNA]
pep2rna tbl = map concat . foldr (\a rs -> [ c : cs | c <- lookup a, cs <- rs ]) [[]]
  where
    lookup :: AminoAcid -> [Codon]
    lookup x = M.findWithDefault [""] x tbl
-}

main = problem2
