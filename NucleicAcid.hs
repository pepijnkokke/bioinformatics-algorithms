module NucleicAcid where

import Data.List ((\\))

type Nucleotide = Char
type DNA        = [Nucleotide]
type RNA        = [Nucleotide]
type Codon      = [Nucleotide]

type AminoAcid  = Char
type Peptide    = [AminoAcid]

-- * Mutations of nucleotides

mutateBy :: (a -> [a]) -> Int -> [a] -> [[a]]
mutateBy _ 0 xs = [xs]
mutateBy _ _ [] = [[]]
mutateBy f n (x:xs) = here ++ further
  where
    here    = [ x' : xs' | x' <- f x, xs' <- mutateBy f (n - 1) xs ]
    further = [ x  : xs' | xs' <- mutateBy f n xs ]

mutateDna :: Nucleotide -> [Nucleotide]
mutateDna n = ['A','T','C','G'] \\ [n]

mutateRna :: Nucleotide -> [Nucleotide]
mutateRna n = ['A','U','C','G'] \\ [n]

-- * Complements of nucleic acids

complementDna :: Nucleotide -> Nucleotide
complementDna 'A' = 'T'
complementDna 'T' = 'A'
complementDna 'C' = 'G'
complementDna 'G' = 'C'
complementDna  c  = error $ "invalid nucleotide " ++ [c]

complementDnaString :: DNA -> DNA
complementDnaString = map complementDna

complementRna :: Nucleotide -> Nucleotide
complementRna 'A' = 'U'
complementRna 'U' = 'A'
complementRna 'C' = 'G'
complementRna 'G' = 'C'
complementRna  c  = error $ "invalid nucleotide " ++ [c]

complementRnaString :: RNA -> RNA
complementRnaString = map complementRna