{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Peter Murphy
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.All (quickCheckAll)
import Data.List.Split
import Data.Maybe
import PrefStringUtil
import Candidate
import Data.Char (isSpace)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit
import System.IO
import System.IO.Unsafe (unsafePerformIO)


-- | This takes a sequence of integers - representing the numbers on a ballot paper - and another
-- integer, representing the number of boxes. This function returns True if this represents a formal
-- ballot paper, which is some permutation of [1, 2.. number of boxes]. It returns False otherwise.
isformal :: [Int] -> Int -> Bool
isformal ballotpaper expectednumber
    | lengththing == 0 = False
    | lengththing /= expectednumber = False
    | sortedballot == [1..lengththing] = True
    | otherwise = False
    where sortedballot = sort ballotpaper
          lengththing = length ballotpaper

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

-- | This method returns the position(s) of the minimum value in the list when it
-- doesn't fall in a list of excluded indices
findlowestposex :: [Int] -> [Int] -> [Int]
findlowestposex seqval excludepos
    | null seqval = []
    | otherwise = [i | i <- otherwiseval, notElem i excludepos]
    where minval = minimum (excludeseq seqval excludepos)
          otherwiseval = elemIndices minval seqval

-- | This method takes a sequence and a list of indices to exclude. It returns another
-- sequence consisting of the remaining values in the original sequence
excludeseq :: [Int] -> [Int] -> [Int]
excludeseq seqval excludepos = ourseqret
    where ourlength = length seqval
          ourrange = [0..(ourlength - 1)]
          ourdesiredindices = [i | i <- ourrange, notElem i excludepos]
          ourseqret = [seqval !! i | i <- ourdesiredindices]

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


tests = [
        testGroup "Testing the iswhitespacewithin function" [
                testCase "wspace1" testWspace1,
                testCase "wspace2" testWspace2,
                testCase "wspace3" testWspace3,
                testCase "wspace4" testWspace4,
                testCase "wspace5" testWspace5,
                testCase "wspace6" testWspace6
            ],
        testGroup "Testing the convertstringtoint function" [
                testCase "convertstringtoint1" testConvertstringtoint1,
                testCase "convertstringtoint2" testConvertstringtoint2,
                testCase "convertstringtoint3" testConvertstringtoint3,
                testCase "convertstringtoint4" testConvertstringtoint4,
                testCase "convertstringtoint5" testConvertstringtoint5,
                testCase "convertstringtoint6" testConvertstringtoint6
            ],
        testGroup "Testing the convertstringtoseq function" [
                testCase "convertstringtoseq1" testPrefline1,
                testCase "convertstringtoseq2" testPrefline2,
                testCase "convertstringtoseq3" testPrefline3,
                testCase "convertstringtoseq4" testPrefline4
            ],
        testGroup "Testing the trim function" [
                testProperty "trim1" propTrim,
                testProperty "trim2" propTrimalso
            ],
        testGroup "Testing the stripandtrim function" [
                testProperty "emptystripandtrim" propStripandtrimempty,
                testProperty "onestripandtrim" propStripandtrimone,
                testProperty "twostripandtrimtwo" propStripandtrimtwo,
                testProperty "twostripandtrimthree" propStripandtrimthree,
                testCase "arbitrarystrip" testStripandtrim
            ],
        testGroup "Testing the candstrin and candstrout functions" [
                testProperty "candstrin" propCandstrin,
                testProperty "candstrout" propCandstrout
            ],
        testGroup "Testing the createcandidatesfromline" [
                testCase "threecand" testCreatecand1,
                testCase "twocand" testCreatecand2
            ],
        testGroup "Testing the countlengths method" [
                testCase "count1" testCountlengths1,
                testCase "count2" testCountlengths2,
                testCase "count3" testCountlengths3,
                testCase "count4" testCountlengths4,
                testCase "count5" testCountlengths5
            ],
        testGroup "Testing the getproportions" [
                testCase "proport1" testGetproportions1,
                testCase "proport2" testGetproportions2,
                testCase "proport3" testGetproportions3,
                testCase "proport4" testGetproportions4,
                testCase "proport5" testGetproportions5,
                testCase "proport6" testGetproportions6
            ]
    ]

getformals :: [Maybe [Int]] -> Int -> [[Int]]
getformals ballotlist nocand  = [fromJust ballot | ballot <- ballotlist,
    isJust ballot, isformal (fromJust ballot) nocand]


getseqwithfirstpref :: [[Int]] -> Int -> [[Int]]
getseqwithfirstpref ballotlist prefpos = [ballot | ballot <- ballotlist,
    findlowestpos ballot == [prefpos]]

getfirstprefs :: [[Int]] -> Int -> [[[Int]]]
getfirstprefs ballotlist nocand = [getseqwithfirstpref ballotlist i | i <- [0..(nocand-1)]]

getseqwithsubpref :: [[[Int]]] -> Int -> [Int] -> Int -> [[Int]]
getseqwithsubpref ballotlist exclindex excludedcandpos prefpos = [ballot | ballot <- ballotlist !! exclindex,
    findlowestposex ballot (exclindex : excludedcandpos) == [prefpos]]

transferballot :: [[[Int]]] -> Int -> Int -> [Int] -> [[[Int]]]
transferballot ballotlist nocand exclindex excludedcandpos =
    [getseqwithsubpref ballotlist exclindex excludedcandpos i | i <- [0..(nocand-1)]]


combtransandorigpiles :: [[[Int]]] -> [[[Int]]] -> Int -> Int -> [[[Int]]]
combtransandorigpiles transballotpile origballotpile nocand excludindex =
    [zipornot transballotpile origballotpile excludindex i | i <- [0..(nocand-1)]]

zipornot :: [[[Int]]] -> [[[Int]]] -> Int -> Int -> [[Int]]
zipornot transballotpile origballotpile excludindex currentindex = if excludindex == currentindex
    then []  else (transballotpile !! currentindex) ++ (origballotpile !! currentindex)


-- | This defines the data structure ElectionStart which represents the starting count of
-- an election.
data Election = Election
    { candidates :: [Maybe Candidate],
      firstballots :: [[[Int]]],
      firstballotcount :: [Int],
      percentagecount :: [Double],
      noballots:: Int,
      noformals:: Int,
      noinformals :: Int,
      iswonyet :: Bool,
      winningcand :: Maybe Candidate}
    deriving (Eq, Show, Read)


-- | The constructelection method is used to create an Election instance from a string (which is
-- the contents of a file).
constructelection :: String -> Maybe Election
constructelection stringval
    | not (null ballotlines) = Just Election{candidates = ourcandidates,
            firstballots = thoseballotpiles,
            firstballotcount = thoseballotcounts,
            percentagecount = thosepercentagecount,
            noballots = length electionseq,
            noformals = length thoseformals,
            noinformals = length electionseq - length thoseformals,
            iswonyet = wonyet,
            winningcand = ourwinningcand}
    | otherwise   = Nothing
    where candidateline:ballotlines = lines stringval
          ourcandidates = createcandidatesfromline candidateline ',' '-'
          nocandidates = length ourcandidates
          electionseq = [convertstringtoseq b | b <- ballotlines]
          thoseformals = getformals electionseq nocandidates
          thoseballotpiles = getfirstprefs thoseformals nocandidates
          thoseballotcounts = countlengths thoseballotpiles
          numformals = length thoseformals
          thosepercentagecount = getproportions thoseballotcounts numformals
          wonyet = iswinningproportion thosepercentagecount
          ourwinningcand
              | not wonyet = Nothing
              | otherwise = ourcandidates !! findlasthighestpos thoseballotcounts

-- Note: tie-breaker - choose the last candidate, to go against the donkey vote.

getminindexes :: [Int] -> [Int] -> [Int]
getminindexes startpos seqtocheck = [i | i <- [0..(nocand-1)], elem i startpos, seqtocheck !! i == minvalue]
    where nocand = length seqtocheck
          minvalue = minimum [seqtocheck !! j | j <- startpos]

getmaxindexes :: [Int] -> [Int] -> [Int]
getmaxindexes startpos seqtocheck = [i | i <- [0..(nocand-1)], elem i startpos, seqtocheck !! i == maxvalue]
    where nocand = length seqtocheck
          maxvalue = maximum [seqtocheck !! j | j <- startpos]


getlosers :: ElectionRound -> [Int] -> Int -> Int
getlosers electionround seqval 2
              | length seqval == 1 = head seqval
              | length minindexes == 1 = head minindexes
              | otherwise = head (getminindexes minindexes (firstballotcount (theelection electionround)))
              where minindexes = getminindexes seqval (latestballotcount electionround)

getlosers electionround seqval n
              | length seqval == 1 = head seqval
              | length minindexes == 1 = head minindexes
              | otherwise =  getlosers (fromJust  thelastround) minindexes (n-1)
              where minindexes = getminindexes seqval (latestballotcount electionround)
                    thelastround = lastround electionround

getwinners :: ElectionRound -> [Int] -> Int -> Int
getwinners electionround seqval 1
              | length seqval == 1 = head seqval
              | length maxindexes == 1 = head maxindexes
              | otherwise = head (getmaxindexes maxindexes (firstballotcount ourelection))
              where ourelection = theelection electionround
                    maxindexes = getmaxindexes seqval (latestballotcount electionround)





getwinners electionround seqval n
              | length seqval == 1 = head seqval
              | length maxindexes == 1 = head maxindexes
              | otherwise =  getwinners (fromJust  thelastround) maxindexes (n-1)
              where maxindexes = getmaxindexes seqval (latestballotcount electionround)
                    thelastround = lastround electionround



data ElectionRound = ElectionRound
    { theelection :: Election,
      lastround :: Maybe ElectionRound,
      roundnumber :: Int,
      excludedcandpos :: [Int],
      candexcludeindex :: Int,
      transballots :: [[[Int]]],
      transballotcount :: [Int],
      transpercentcount :: [Double],
      latestballots :: [[[Int]]],
      latestballotcount :: [Int],
      latestpercentcount :: [Double],
      haswonyet :: Bool,
      thewinningcand :: Maybe Candidate}
    deriving (Eq, Show, Read)


runelectionround :: Election -> Int -> ElectionRound

runelectionround ourelection 1 = ElectionRound {
      theelection = ourelection,
      lastround = Nothing,
      roundnumber = 1,
      excludedcandpos = [],
      candexcludeindex = exclindex,
      transballots = ourtransballots,
      transballotcount = countlengths ourtransballots,
      transpercentcount = getproportions ourtransballotcount noballots,
      latestballots = ourlatestballots,
      latestballotcount = ourlatestballotcount,
      latestpercentcount = ourpercentcount,
      haswonyet = wonyet,
      thewinningcand = ourcand}
     where ourstartballotpile = firstballots ourelection
           ourcands = candidates ourelection
           nocand = length (candidates ourelection)
           noballots = noformals ourelection
           exclindex = findfirstlowestpos (firstballotcount ourelection)
           ourtransballots = transferballot ourstartballotpile nocand exclindex []
           ourtransballotcount = countlengths ourtransballots
           ourlatestballots = combtransandorigpiles ourstartballotpile ourtransballots nocand exclindex
           ourlatestballotcount = countlengths ourlatestballots
           ourpercentcount = getproportions ourlatestballotcount noballots
           highestpositions = findhighestpos ourlatestballotcount
           wonyet = if iswonyet ourelection then True else iswinningproportion ourpercentcount
           ourcand = if isNothing (winningcand ourelection)
                        then if wonyet
                                 then if length highestpositions == 1
                                     then ourcands !! (head highestpositions)
                                     else if firstballots ourelection !! (head highestpositions) >  firstballots ourelection !! (highestpositions !! 1)
                                         then ourcands !! (head highestpositions)
                                         else ourcands !! (highestpositions !! 1)
                                 else Nothing
                        else winningcand ourelection

-- Two tie breakers: first, findfirstlowestpos to get loser.
-- Secondly, find last person to be eliminated.




runelectionround ourelection n = ElectionRound {
      theelection = ourelection,
      lastround = Just thelastround,
      roundnumber = n,
      excludedcandpos = ourexclusions,
      candexcludeindex = exclindex,
      transballots = ourtransballots,
      transballotcount = ourtransballotcount,
      transpercentcount = getproportions ourtransballotcount noballots,
      latestballots = ourlatestballots,
      latestballotcount = countlengths ourlatestballots,
      latestpercentcount = ourlatestpercentcount,
      haswonyet = wonyet,
      thewinningcand = ourcand}
     where thelastround = runelectionround ourelection (n-1)
           ourcands = candidates ourelection
           nocand = length (candidates ourelection)
           ourexclusions = candexcludeindex thelastround : excludedcandpos thelastround
           ourstartballotpile = latestballots thelastround
           noballots = noformals ourelection
           ourlowestpositions = findlowestposex (latestballotcount thelastround) ourexclusions
           exclindex = getlosers thelastround ourlowestpositions n
           ourtransballots = transferballot ourstartballotpile nocand exclindex ourexclusions
           ourtransballotcount = countlengths ourtransballots
           ourlatestballots = combtransandorigpiles ourstartballotpile ourtransballots nocand exclindex
           ourlatestballotcount = countlengths ourlatestballots
           ourlatestpercentcount = getproportions ourlatestballotcount noballots
           highestpositions = findhighestpos ourlatestballotcount
           wonyet = if haswonyet thelastround then True else iswinningproportion ourlatestpercentcount
           ourcand = if isNothing (winningcand ourelection)
                        then if wonyet
                                 then if length highestpositions == 1
                                     then ourcands !! (head highestpositions)
                                     else ourcands !! (getwinners thelastround highestpositions (n-1))
                                 else Nothing
                        else winningcand ourelection

getelectionrounds :: ElectionRound -> [ElectionRound]
getelectionrounds electionround = if isNothing (lastround electionround)
                                      then [electionround]
                                      else electionround : getelectionrounds (fromJust (lastround electionround))


data ElectionResults = ElectionResults
    { thiselection :: Election,
      thisrounds :: [ElectionRound]}
    deriving (Eq, Show, Read)


constructelectionresults :: String -> Maybe ElectionResults
constructelectionresults stringval
    | not (isNothing ourelection) && numbercand > 2 = Just ElectionResults{thiselection = fromJust ourelection,
        thisrounds = reverse (getelectionrounds (runelectionround (fromJust ourelection) (numbercand - 2)))}
    | not (isNothing ourelection) && numbercand <= 2 = Just ElectionResults{thiselection = fromJust ourelection,
        thisrounds = []}
    | otherwise = Nothing
    where ourelection = constructelection stringval
          numbercand =  length (candidates (fromJust ourelection))



testfile' :: String -> IO (Maybe ElectionResults)
testfile' strval = do
    ourhandle <- openFile strval ReadMode
    hSetEncoding ourhandle utf8
    ourstroutput <- hGetContents ourhandle
    let ourmayberesults = constructelectionresults ourstroutput
--    hClose h
    return ourmayberesults

testfile :: String -> (Maybe ElectionResults)
testfile strval = unsafePerformIO (testfile' strval)


-- Entry point for unit tests.
testMain = defaultMain tests

-- The main program. Asks for a file of electoral data, decides the winner, and
-- tells the user.

exeMain = do
    let q = testfile "testdata/melbourne2013.txt"
    print q



--    putStrLn("Good day. What file full of election data would you like to count?")
--    filename <- getLine
--    putStrLn("Would you like to find the [w]inner or count [f]ull preferences?")
--    choice <- getLine
--    let willcountpref = if (length choice > 0) && (elem (head choice) "Ff" ) then True else False
--    putStrLn("Good bye!")



-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

