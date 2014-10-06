module PrefSeqUtil where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.All (quickCheckAll)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit
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
iswinningproportion seqval = any (\k -> k >= fromIntegral 1 / fromIntegral 2) seqval

getminindexes :: [Int] -> [Int] -> [Int]
getminindexes startpos seqtocheck = [i | i <- [0..(nocand-1)], elem i startpos, seqtocheck !! i == minvalue]
    where nocand = length seqtocheck
          minvalue = minimum [seqtocheck !! j | j <- startpos]

getmaxindexes :: [Int] -> [Int] -> [Int]
getmaxindexes startpos seqtocheck = [i | i <- [0..(nocand-1)], elem i startpos, seqtocheck !! i == maxvalue]
    where nocand = length seqtocheck
          maxvalue = maximum [seqtocheck !! j | j <- startpos]






-- This function tests the findhighestpos function

testFindhighestpos1 = findhighestpos [] @?= []
testFindhighestpos2 = findhighestpos [0] @?= [0]
testFindhighestpos3 = findhighestpos [0,0] @?= [0,1]
testFindhighestpos4 = findhighestpos [0,1] @?= [1]
testFindhighestpos5 = findhighestpos [1,0] @?= [0]
testFindhighestpos6 = findhighestpos [1,1] @?= [0,1]

-- This function tests the findlasthighestpos function

testFindlasthighestpos2 = findlasthighestpos [0] @?= 0
testFindlasthighestpos3 = findlasthighestpos [0,0] @?= 1
testFindlasthighestpos4 = findlasthighestpos [0,1] @?= 1
testFindlasthighestpos5 = findlasthighestpos [1,0] @?= 0
testFindlasthighestpos6 = findlasthighestpos [1,1] @?= 1

-- This function tests the findlowestpos function

testFindlowestpos1 = findlowestpos [] @?= []
testFindlowestpos2 = findlowestpos [0] @?= [0]
testFindlowestpos3 = findlowestpos [0,0] @?= [0,1]
testFindlowestpos4 = findlowestpos [0,1] @?= [0]
testFindlowestpos5 = findlowestpos [1,0] @?= [1]
testFindlowestpos6 = findlowestpos [1,1] @?= [0,1]

-- This function tests the findfirstlowestpos function

testFindfirstlowestpos2 = findfirstlowestpos [0] @?= 0
testFindfirstlowestpos3 = findfirstlowestpos [0,0] @?= 0
testFindfirstlowestpos4 = findfirstlowestpos [0,1] @?= 0
testFindfirstlowestpos5 = findfirstlowestpos [1,0] @?= 1
testFindfirstlowestpos6 = findfirstlowestpos [1,1] @?= 0

-- This function tests the countlengths function

testCountlengths1 = countlengths [] @?= []
testCountlengths2 = countlengths [[]] @?= [0]
testCountlengths3 = countlengths [[1]] @?= [1]
testCountlengths4 = countlengths [[[1]], [[2]]] @?= [1, 1]
testCountlengths5 = countlengths [[1, 2], [1]] @?= [2, 1]

-- This function tests the getproportions method

testGetproportions1 = getproportions [] 2 @?= []
testGetproportions2 = getproportions [] 3 @?= []
testGetproportions3 = getproportions [1] 2 @?= [fromIntegral 1 / fromIntegral 2]
testGetproportions4 = getproportions [1] 3 @?= [fromIntegral 1 / fromIntegral 3]
testGetproportions5 = getproportions [2, 3] 2 @?= [fromIntegral 2 / fromIntegral 2,
    fromIntegral 3 / fromIntegral 2]
testGetproportions6 = getproportions [2, 3] 3 @?= [fromIntegral 2 / fromIntegral 3,
    fromIntegral 3 / fromIntegral 3]

