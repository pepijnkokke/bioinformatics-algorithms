{-# LANGUAGE FlexibleInstances #-}
module OriC where

import Control.Arrow ((***))
import Data.List (sortBy,findIndices,nub,(\\))
import Data.List.Split (chunksOf)
import Data.Map (Map,toList,fromListWith,unionWith,mapKeys)
import Data.String.Utils (strip)
import System.IO (readLn)



-- * Most frequent word problem

-- |Find the most frequent k-mers in a string.
--  Input: A string Text and an integer k.
--  Output: All most frequent k-mers in Text.
problem1 :: IO ()
problem1 = do
  str <- readLn
  k   <- readLn

  putStrLn . unwords . mostFrequent . frequencyMap . kmers k $ str

mostFrequent :: Ord a => Map [a] Int -> [[a]]
mostFrequent = map fst
               . takeWhile2 (\(_,x)(_,y) -> x == y)
               . sortBy (\(_,x)(_,y) -> compare y x)
               . toList

-- |Compute the frequency table for the elements in a list.
frequencyMap :: Ord a => [a] -> Map a Int
frequencyMap xs = fromListWith (+) [(x, 1) | x <- xs]

-- |Compute all k-mers in a string.
kmers :: Int -> [a] -> [[a]]
kmers k xs
  | length xs >= k = take k xs : kmers k (tail xs)
  | otherwise     = []

-- |Take elements from a list as long as some relation holds.
takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 rel (x:xs) = x : takeWhile2' rel x xs
  where
    takeWhile2' :: (a -> a -> Bool) -> a -> [a] -> [a]
    takeWhile2' _ _ [] = []
    takeWhile2' rel x (y:ys)
      | x `rel` y = y : takeWhile2' rel y ys
      | otherwise = []



-- * Reverse complement problem

-- |Reverse complement a nucleotide pattern.
--  Input: A DNA string Pattern.
--  Output: Pattern, the reverse complement of Pattern.
problem2 :: IO ()
problem2 = putStrLn . reverse . complement =<< getLine

type Base = Char

class HasComplement a where
  complement :: a -> a
  isComplement :: Eq a => a -> a -> Bool
  isComplement x y = x == complement y

instance HasComplement Base where
  complement 'A' = 'T'
  complement 'T' = 'A'
  complement 'C' = 'G'
  complement 'G' = 'C'
  complement  c  = error $ "invalid base " ++ [c]

instance HasComplement a => HasComplement [a] where
  complement = fmap complement



-- * Pattern matching problem

-- |Find all occurrences of a pattern in a string.
--  Input: Two strings: a pattern and a genome string.
--  Output: All starting positions where pattern appears as a substring of genome.
problem3 :: IO ()
problem3 = do
  patn <- getLine
  str  <- getLine
  putStrLn . unwords . map show . matches patn $ str

matches :: Eq a => [a] -> [a] -> [Int]
matches = matchesBy (==)

matchesBy :: ([a] -> [a] -> Bool) -> [a] -> [a] -> [Int]
matchesBy rel patn str = findIndices (rel patn) (kmers (length patn) str)



-- * Clump finding problem

-- |Find patterns forming clumps in a string.
--  Input: a string genome, and integers k, l, and t.
--  Output: all distinct k-mers forming (l, t)-clumps in genome.
problem4 :: IO ()
problem4 = do
  str     <- readLn
  [k,l,t] <- return . map read . words =<< readLn
  putStrLn . unwords . clumps k l t $ str

-- |Find patterns forming clumps in a string.
clumps :: Ord a => Int -> Int -> Int -> [a] -> [[a]]
clumps k l t = nub
               . concatMap (map fst
                            . takeWhile (\x -> snd x >= t)
                            . frequencyList
                            . kmers k)
               . kmers l

-- |Compute the ordered frequency list.
frequencyList :: Ord a => [a] -> [(a,Int)]
frequencyList = sortBy (\(_,x)(_,y) -> compare y x) . toList . frequencyMap



-- * Minimal skew problem

-- |Find a position in a genome minimizing the skew.
--  Input: A DNA string Genome.
--  Output: All integer(s) i minimizing Skew among all values of i (from 0 to |Genome|).
problem5 :: IO ()
problem5 = putStrLn . unwords . map show . reverse . skew =<< getLine


skew :: [Base] -> [Int]
skew = skewAcc 0 [] maxBound 0
  where
    skewAcc :: Int -> [Int] -> Int -> Int -> [Base] -> [Int]
    skewAcc _ ix _ _ [      ] = ix
    skewAcc i ix m s ('C':bs)
      | s <= m = skewAcc (i + 1) [] (s - 1) (s - 1) bs
      | s > m = skewAcc (i + 1) ix m (s - 1) bs
    skewAcc i ix m s ('G':bs)
      | s == m = skewAcc (i + 1) (i:ix) m (s + 1) bs
      | s > m = skewAcc (i + 1) ix m (s + 1) bs
    skewAcc i ix m s ( _ :bs)
              = skewAcc (i + 1) ix m s bs



-- * Approximate pattern matching problem

-- |Find all approximate occurrences of a pattern in a string.
--  Input: Two strings Pattern and Text along with an integer d.
--  Output: All positions where Pattern appears in Text with at most d mismatches.
problem6 :: IO ()
problem6 = do
  patn <- getLine
  str  <- getLine
  d    <- readLn
  putStrLn . unwords . map show . matchesBy (approx d) patn $ str

approx :: Eq a => Int -> [a] -> [a] -> Bool
approx d xs ys = diffs xs ys <= d

diffs :: Eq a => [a] -> [a] -> Int
diffs [] ys = length ys
diffs xs [] = length xs
diffs (x:xs) (y:ys)
  | x == y = 0 + diffs xs ys
  | x /= y = 1 + diffs xs ys



-- * Frequent Words with Mismatches Problem

-- |Find the most frequent k-mers with mismatches in a string.
--  Input: A string Text as well as integers k and d. (You may assume k <= 12 and d <= 3).
--  Output: All most frequent k-mers with up to d mismatches in Text.
problem7 :: IO ()
problem7 = do
  [str,k',d'] <- return . words =<< getLine
  let k = read k'
  let d = read d'

  let mostFreq = mostFrequent . approxFrequencyMap mutateBase d . kmers k $ str

  putStrLn . unwords $ mostFreq

approxFrequencyMap :: Ord a => (a -> [a]) -> Int -> [[a]] -> Map [a] Int
approxFrequencyMap f d = frequencyMap . concatMap (mutateBy f d)

mutateBy :: (a -> [a]) -> Int -> [a] -> [[a]]
mutateBy _ 0 xs = [xs]
mutateBy _ _ [] = [[]]
mutateBy f n (x:xs) = now ++ later
  where
    now   = [ x' : xs' | x' <- f x, xs' <- mutateBy f (n - 1) xs ]
    later = [ x  : xs' | xs' <- mutateBy f n xs ]

mutateBase :: Base -> [Base]
mutateBase b = ['A','T','C','G'] \\ [b]

-- * Frequent Words with Mismatches and Reverse Complements Problem

-- |Find the most frequent k-mers (with mismatches and reverse complements) in a DNA string.
--  Input: A DNA string Text as well as integers k and d.
--  Output: All k-mers Pattern maximizing the sum Count(Text, Pattern) + Count(Text, Pattern) over all possible k-mers.
problem8 :: IO ()
problem8 = do
  str <- getLine
  [k,d] <- return . map read . words =<< getLine

  let freqMap1 = approxFrequencyMap mutateBase d . kmers k $ str
  let freqMap2 = withReverseComplement freqMap1
  let freqList = sortBy (\(_,x)(_,y) -> compare y x) . toList $ freqMap2
  let mostFreq = takeWhile2 (\(_,x)(_,y) -> x == y) $ freqList

  putStrLn . unwords . map fst $ mostFreq

withReverseComplement :: Map [Base] Int -> Map [Base] Int
withReverseComplement m = unionWith (+) m (mapKeys (reverse . complement) m)
