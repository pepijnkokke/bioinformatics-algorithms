{-# LANGUAGE FlexibleInstances #-}
import Control.Arrow ((***))
import Data.List (sortBy,findIndices,nub)
import Data.List.Split (chunksOf)
import Data.Map (Map,toList,fromListWith)
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
  putStrLn . unwords . map fst . mostFrequent . kmers k $ str


-- |Compute the most frequent elements in a list.
mostFrequent :: Ord a => [a] -> [(a,Int)]
mostFrequent = takeBy (\x y -> snd x == snd y) . frequencyList


-- |Take elements from a list as long as some relation holds.
takeBy :: (a -> a -> Bool) -> [a] -> [a]
takeBy _ [] = []
takeBy rel (x:xs) = x : takeBy' rel x xs
  where
    takeBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
    takeBy' _ _ [] = []
    takeBy' rel x (y:ys)
      | x `rel` y = y : takeBy' rel y ys
      | otherwise = []

-- |Compute an ordered frequency list.
frequencyList :: Ord a => [a] -> [(a,Int)]
frequencyList = sortBy (\x y -> compare (snd y) (snd x)) . frequencyMap


-- |Compute the frequency table for the elements in a list.
frequencyMap :: Ord a => [a] -> [(a,Int)]
frequencyMap xs = toList . fromListWith (+) [(x, 1) | x <- xs]

-- |Compute all k-mers in a string.
kmers :: Int -> [a] -> [[a]]
kmers k xs
  | length xs >= k = take k xs : kmers k (tail xs)
  | otherwise     = []



-- * Reverse complement problem
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

instance HasComplement a => HasComplement [a] where
  complement = fmap complement



-- * Pattern matching problem

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

-- |find patterns forming clumps in a string.
--  input: a string genome, and integers k, l, and t.
--  output: all distinct k-mers forming (l, t)-clumps in genome.
problem4 :: IO ()
problem4 = do
  str     <- readLn
  [k,l,t] <- return . map read . words =<< readLn
  putStrLn . unwords . clumps k l t $ str


clumps :: Ord a => Int -> Int -> Int -> [a] -> [[a]]
clumps k l t = nub . concatMap (map fst . takeWhile (\x -> snd x >= t) . frequencyList . kmers k) . kmers l



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
problem7 = undefined

approxFrequencyMap :: Int -> [a] -> [(a,Int)]
approxFrequencyMap d = undefined

main = problem7
