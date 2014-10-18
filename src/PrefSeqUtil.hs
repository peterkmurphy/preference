module PrefSeqUtil where

import Data.List
import Data.Maybe

-- | This method takes a sequence and a list of indices to exclude. It returns another
-- sequence consisting of the remaining values in the original sequence
excludeseq :: [Int] -> [Int] -> [Int]
excludeseq seqval excludepos = ourseqret
    where ourlength = length seqval
          ourrange = [0..(ourlength - 1)]
          ourdesiredindices = [i | i <- ourrange, notElem i excludepos]
          ourseqret = [seqval !! i | i <- ourdesiredindices]

-- | This method returns the position(s) of the maximum value in the list.
findhighestpos :: [Int] -> [Int]
findhighestpos seqval
    | null seqval = []
    | otherwise = elemIndices maxval seqval
    where maxval = maximum seqval

-- | This method returns the last position for the maximum value in the list.
findlasthighestpos :: [Int] -> Int
findlasthighestpos seqval = last (findhighestpos seqval)

-- | This method returns the position(s) of the minimum value in the list.
findlowestpos :: [Int] -> [Int]
findlowestpos seqval
    | null seqval = []
    | otherwise = elemIndices minval seqval
    where minval = minimum seqval

-- | This method returns the first position for the minimum value in the list.
findfirstlowestpos :: [Int] -> Int
findfirstlowestpos seqval = head (findlowestpos seqval)
-- We need to test convertstringtoseq as well.

-- | This method returns the position(s) of the minimum value in the list when it
-- doesn't fall in a list of excluded indices
findlowestposex :: [Int] -> [Int] -> [Int]
findlowestposex seqval excludepos
    | null seqval = []
    | otherwise = [i | i <- otherwiseval, notElem i excludepos]
    where minval = minimum (excludeseq seqval excludepos)
          otherwiseval = elemIndices minval seqval

-- | This method takes a list of lists, and returns a list consisting of the the length
-- of each list therein.
countlengths :: [[a]] -> [Int]
countlengths seqofseqs = [length i | i <- seqofseqs]

-- | This method takes a list of integers, and a value representing the "total". This method returns
-- another list represents the proportion of each list item relative to the total.
getproportions :: [Int] -> Int -> [Double]
getproportions seqofints total =
    [fromIntegral p / fromIntegral total| p <- seqofints]

-- | This method checks if there is a "winning" proportion - one equal to or exceeding 50%.
iswinningproportion :: [Double] -> Bool
iswinningproportion seqval = any (\k -> k >= 1 / 2) seqval

-- | This method starts with a initial sequence of positions to check in a sequence. Having found
-- the minimum value for all positions checked in the sequence, the method returns a sequence of
-- those positions corresponding to the minimum. (This method filters the initial sequence of
-- minimums, but does not add extra indices in the process).
filterminindexes :: [Int] -> [Int] -> [Int]
filterminindexes initialposns seqtocheck = [i | i <- [0..(nocand-1)], elem i initialposns,
    seqtocheck !! i == minvalue]
    where nocand = length seqtocheck
          minvalue = minimum [seqtocheck !! j | j <- initialposns `intersect` [0..(nocand-1)]]

-- | This method starts with a initial sequence of positions to check in a sequence. Having found
-- the maximum value for all positions checked in the sequence, the method returns a sequence of
-- those positions corresponding to the maximum. (This method filters the initial sequence of
-- maximums, but does not add extra indices in the process).
filtermaxindexes :: [Int] -> [Int] -> [Int]
filtermaxindexes initialposns seqtocheck = [i | i <- [0..(nocand-1)], elem i initialposns,
    seqtocheck !! i == maxvalue]
    where nocand = length seqtocheck
          maxvalue = maximum [seqtocheck !! j | j <- initialposns `intersect` [0..(nocand-1)]]

