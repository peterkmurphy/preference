module ElectionClasses where

import Data.List
import Control.Exception
import Test.QuickCheck
import Test.QuickCheck.All (quickCheckAll)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import PrefStringUtil
import PrefSeqUtil
import Candidate
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

testfile'' :: String -> IO (Maybe ElectionResults)
testfile'' strval = do
    result <- try (testfile' strval) :: IO (Either SomeException (Maybe ElectionResults))
    case result of
        Left ex  -> return Nothing
        Right val -> return val

testfile :: String -> Maybe ElectionResults
testfile strval =  unsafePerformIO (testfile'' strval)


pseudoequalelection :: Election -> Election -> Bool
pseudoequalelection elect1 elect2
    | candidates elect1 == candidates elect2 &&
      firstballotcount elect1 == firstballotcount elect2 &&
      noballots elect1 == noballots elect2 &&
      noformals elect1 == noformals elect2 &&
      iswonyet elect1 == iswonyet elect2 &&
      winningcand elect1 == winningcand elect2 = True
    | otherwise = False


pseudoequaleround :: ElectionRound -> ElectionRound -> Bool
pseudoequaleround eround1 eround2
    | roundnumber eround1 == roundnumber eround2 &&
      candexcludeindex eround1 == candexcludeindex eround2 &&
      transballotcount eround1 == transballotcount eround2 &&
      latestballotcount eround1 == latestballotcount eround2 &&
      haswonyet eround1 == haswonyet eround2 &&
      thewinningcand eround1 == thewinningcand eround2 = True
    | otherwise = False

pseudoeresults :: Maybe ElectionResults -> Maybe ElectionResults -> Bool
pseudoeresults eresults1 eresults2
    | isNothing eresults1 && isNothing eresults2 = True
    | not (isNothing eresults1) && not (isNothing eresults2) &&
      (pseudoequalelection (thiselection arg1) (thiselection arg2)) &&
      length (thisrounds arg1) == length (thisrounds arg2) &&
      all (\x -> thisrounds arg1 !! x == thisrounds arg2 !! x)
          [0..deslength] = True
    | otherwise = False
    where arg1 = fromJust eresults1
          arg2 = fromJust eresults2
          deslength = length (thisrounds arg1)



