module OurTests.TestStuff where

import Data.List
import Data.Maybe
import PrefStringUtil
import PrefSeqUtil
import Candidate
import ElectionClasses
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

-- This code tests PrefStringUtil.hs.

-- We need to test iswhitespacewithin.

testWspace1 = iswhitespacewithin ""  @?= False
testWspace2 = iswhitespacewithin " "  @?= True
testWspace3 = iswhitespacewithin "A"  @?= False
testWspace4 = iswhitespacewithin "A\n"  @?= True
testWspace5 = iswhitespacewithin "\nA"  @?= True
testWspace6 = iswhitespacewithin spacerange  @?= True

-- This code tests the trim method.

propTrim :: String -> Property
propTrim s = not (iswhitespacewithin s) ==> trim (spacerange ++ s ++ spacerange) == s

propTrimalso :: String -> Property
propTrimalso s = not (iswhitespacewithin s)  && not (null s)
    ==> trim (spacerange ++ s ++ spacerange ++ s ++ spacerange) == s ++ spacerange ++ s

-- This code tests the teststripandtrim method.

propStripandtrimempty :: Char -> Bool
propStripandtrimempty c = stripandtrim "" c == [""]

propStripandtrimone :: String -> Char -> Property
propStripandtrimone s c = not (iswhitespacewithin s)  && not (null s) && notElem c s
    ==> stripandtrim s c == [s]

propStripandtrimtwo :: String -> String -> Char -> Property
propStripandtrimtwo s t c = not (iswhitespacewithin s) && not (iswhitespacewithin t) && not (null s)
    && notElem c s && not (null t) && notElem c t
    ==> stripandtrim (s ++ [c] ++ t) c == [s, t]

propStripandtrimthree :: String -> String -> String -> Char -> Property
propStripandtrimthree s t u c = not (iswhitespacewithin s) && not (iswhitespacewithin t) && not (null s)
    && notElem c s && not (null t) && notElem c t && not (null u) && notElem c u &&
    not (iswhitespacewithin u) ==> stripandtrim (s ++ [c] ++ t ++ [c] ++ u) c == [s, t, u]

testStripandtrim = stripandtrim "1, 2, 3, 4, 6, 5, 7" ',' @?= ["1", "2", "3", "4", "6", "5", "7"]

-- This tests convertstringtoint

testConvertstringtoint1 = convertstringtoint ""  @?= Nothing
testConvertstringtoint2 = convertstringtoint " "  @?= Nothing
testConvertstringtoint3 = convertstringtoint "A"  @?= Nothing
testConvertstringtoint4 = convertstringtoint "1"  @?= Just 1
testConvertstringtoint5 = convertstringtoint "-1"  @?= Just (-1)
testConvertstringtoint6 = convertstringtoint spacerange  @?= Nothing

-- We need to test convertstringtoseq as well.

testPrefline1 = convertstringtoseq "1, 2, 3" @?= Just [1, 2, 3]
testPrefline2 = convertstringtoseq "2" @?= Just [2]
testPrefline3 = convertstringtoseq "" @?= Nothing
testPrefline4 = convertstringtoseq "1, 2, a" @?= Nothing

-- This code tests PrefSeqUtil.hs.

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
testGetproportions3 = getproportions [1] 2 @?= [1 / 2]
testGetproportions4 = getproportions [1] 3 @?= [1 / 3]
testGetproportions5 = getproportions [2, 3] 2 @?= [2 / 2, 3 / 2]
testGetproportions6 = getproportions [2, 3] 3 @?= [2 / 3, 3 / 3]

-- This tests the iswinningproportion method.
testIsWinprop1 = iswinningproportion [] @?= False
testIsWinprop2 = iswinningproportion [0/1] @?= False
testIsWinprop3 = iswinningproportion [1/1] @?= True
testIsWinprop4 = iswinningproportion [1/2] @?= True
testIsWinprop5 = iswinningproportion [1/3] @?= False
testIsWinprop6 = iswinningproportion [1/3, 2/3] @?= True

-- This function tests the filterminindexes function

testFilterminindexes1 = filterminindexes [] [] @?= []
testFilterminindexes2 = filterminindexes [0] [] @?= []
testFilterminindexes3 = filterminindexes [0,1] [] @?= []
testFilterminindexes4 = filterminindexes [1] [] @?= []
testFilterminindexes5 = filterminindexes [1,2] []  @?= []
testFilterminindexes6 = filterminindexes [2] [] @?= []
testFilterminindexes7 = filterminindexes [2, 0] []  @?= []
testFilterminindexes8 = filterminindexes [0, 1, 2] [] @?= []
testFilterminindexes9 = filterminindexes [-1] [] @?= []
testFilterminindexes10 = filterminindexes [0, 1, 2] [1, 1, 1] @?= [0, 1, 2]
testFilterminindexes11 = filterminindexes [] [1, 2, 1] @?= []
testFilterminindexes12 = filterminindexes [0] [1, 2, 1] @?= [0]
testFilterminindexes13 = filterminindexes [0,1] [1, 2, 1] @?= [0]
testFilterminindexes14 = filterminindexes [1] [1, 2, 1] @?= [1]
testFilterminindexes15 = filterminindexes [1,2] [1, 2, 1]  @?= [2]
testFilterminindexes16 = filterminindexes [2] [1, 2, 1] @?= [2]
testFilterminindexes17 = filterminindexes [2, 0] [1, 2, 1]  @?= [0, 2]
testFilterminindexes18 = filterminindexes [0, 1, 2] [1, 2, 1] @?= [0, 2]
testFilterminindexes19 = filterminindexes [-1] [1, 2, 1] @?= []
testFilterminindexes20 = filterminindexes [0, 1, 2, 3] [1, 1, 1] @?= [0, 1, 2]
testFilterminindexes21 = filterminindexes [] [2, 1, 2] @?= []
testFilterminindexes22 = filterminindexes [0] [2, 1, 2] @?= [0]
testFilterminindexes23 = filterminindexes [0,1] [2, 1, 2] @?= [1]
testFilterminindexes24 = filterminindexes [1] [2, 1, 2] @?= [1]
testFilterminindexes25 = filterminindexes [1,2] [2, 1, 2]  @?= [1]
testFilterminindexes26 = filterminindexes [2] [2, 1, 2] @?= [2]
testFilterminindexes27 = filterminindexes [2, 0] [2, 1, 2]  @?= [0, 2]
testFilterminindexes28 = filterminindexes [0, 1, 2] [2, 1, 2] @?= [1]
testFilterminindexes29 = filterminindexes [-1] [2, 1, 2] @?= []

-- This function tests the filtermaxindexes function

testFiltermaxindexes1 = filtermaxindexes [] [] @?= []
testFiltermaxindexes2 = filtermaxindexes [0] [] @?= []
testFiltermaxindexes3 = filtermaxindexes [0,1] [] @?= []
testFiltermaxindexes4 = filtermaxindexes [1] [] @?= []
testFiltermaxindexes5 = filtermaxindexes [1,2] []  @?= []
testFiltermaxindexes6 = filtermaxindexes [2] [] @?= []
testFiltermaxindexes7 = filtermaxindexes [2, 0] []  @?= []
testFiltermaxindexes8 = filtermaxindexes [0, 1, 2] [] @?= []
testFiltermaxindexes9 = filtermaxindexes [-1] [] @?= []
testFiltermaxindexes10 = filtermaxindexes [0, 1, 2] [1, 1, 1] @?= [0, 1, 2]
testFiltermaxindexes11 = filtermaxindexes [] [1, 2, 1] @?= []
testFiltermaxindexes12 = filtermaxindexes [0] [1, 2, 1] @?= [0]
testFiltermaxindexes13 = filtermaxindexes [0,1] [1, 2, 1] @?= [1]
testFiltermaxindexes14 = filtermaxindexes [1] [1, 2, 1] @?= [1]
testFiltermaxindexes15 = filtermaxindexes [1,2] [1, 2, 1]  @?= [1]
testFiltermaxindexes16 = filtermaxindexes [2] [1, 2, 1] @?= [2]
testFiltermaxindexes17 = filtermaxindexes [2, 0] [1, 2, 1]  @?= [0, 2]
testFiltermaxindexes18 = filtermaxindexes [0, 1, 2] [1, 2, 1] @?= [1]
testFiltermaxindexes19 = filtermaxindexes [-1] [1, 2, 1] @?= []
testFiltermaxindexes20 = filtermaxindexes [0, 1, 2, 3] [1, 1, 1] @?= [0, 1, 2]
testFiltermaxindexes21 = filtermaxindexes [] [2, 1, 2] @?= []
testFiltermaxindexes22 = filtermaxindexes [0] [2, 1, 2] @?= [0]
testFiltermaxindexes23 = filtermaxindexes [0,1] [2, 1, 2] @?= [0]
testFiltermaxindexes24 = filtermaxindexes [1] [2, 1, 2] @?= [1]
testFiltermaxindexes25 = filtermaxindexes [1,2] [2, 1, 2]  @?= [2]
testFiltermaxindexes26 = filtermaxindexes [2] [2, 1, 2] @?= [2]
testFiltermaxindexes27 = filtermaxindexes [2, 0] [2, 1, 2]  @?= [0, 2]
testFiltermaxindexes28 = filtermaxindexes [0, 1, 2] [2, 1, 2] @?= [0,2]
testFiltermaxindexes29 = filtermaxindexes [-1] [2, 1, 2] @?= []

-- This contains test code for Candidate.hs.

-- This function tests the candstrin function.

propCandstrin :: String -> String -> Char -> Property
propCandstrin s t c = not (iswhitespacewithin s) && not (iswhitespacewithin t) && not (null s)
    && notElem c s && not (null t) && notElem c t
    ==> fromJust (candstrin (s ++ [c] ++ t) c) == Candidate {name= s, party= t}

-- This function tests the candstrout function.

propCandstrout :: String -> String -> Char -> Property
propCandstrout s t c = not (iswhitespacewithin s) && not (iswhitespacewithin t) && not (null s)
    && notElem c s && not (null t) && notElem c t
    ==> candstrout Candidate {name= s, party= t} c == s ++ [c] ++ t

-- This tests the createcandidatesfromline function

testCreatecand1 =
    createcandidatesfromline "Jules Felix Zanetti - Labor, Will Kitching - The Greens, Tony Abbott - Liberal\n" ',' '-'
    @?= [Just Candidate {name="Jules Felix Zanetti", party= "Labor"},
    Just Candidate {name="Will Kitching", party="The Greens"},
    Just Candidate {name="Tony Abbott", party="Liberal"}]
testCreatecand2 = createcandidatesfromline "Anthony Khouri - Liberal, Jason Clare - Labor" ',' '-'
    @?= [Just Candidate {name="Anthony Khouri", party= "Liberal"},
    Just Candidate {name="Jason Clare", party="Labor"}]

-- This contains all the tests used for this module.

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
        testGroup "Testing the findhighestpos" [
                testCase "findhighestpos1" testFindhighestpos1,
                testCase "findhighestpos2" testFindhighestpos2,
                testCase "findhighestpos3" testFindhighestpos3,
                testCase "findhighestpos4" testFindhighestpos4,
                testCase "findhighestpos5" testFindhighestpos5,
                testCase "findhighestpos6" testFindhighestpos6
            ],
        testGroup "Testing the findlasthighestpos" [
                testCase "findlasthighestpos2" testFindlasthighestpos2,
                testCase "findlasthighestpos3" testFindlasthighestpos3,
                testCase "findlasthighestpos4" testFindlasthighestpos4,
                testCase "findlasthighestpos5" testFindlasthighestpos5,
                testCase "findlasthighestpos6" testFindlasthighestpos6
            ],
        testGroup "Testing the findlowestpos" [
                testCase "findlowestpos1" testFindlowestpos1,
                testCase "findlowestpos2" testFindlowestpos2,
                testCase "findlowestpos3" testFindlowestpos3,
                testCase "findlowestpos4" testFindlowestpos4,
                testCase "findlowestpos5" testFindlowestpos5,
                testCase "findlowestpos6" testFindlowestpos6
            ],
        testGroup "Testing the findfirstlowestpos" [
                testCase "findfirstlowestpos2" testFindfirstlowestpos2,
                testCase "findfirstlowestpos3" testFindfirstlowestpos3,
                testCase "findfirstlowestpos4" testFindfirstlowestpos4,
                testCase "findfirstlowestpos5" testFindfirstlowestpos5,
                testCase "findfirstlowestpos6" testFindfirstlowestpos6
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
            ],
        testGroup "Testing the getproportions" [
                testCase "testIsWinprop1" testIsWinprop1,
                testCase "testIsWinprop2" testIsWinprop2,
                testCase "testIsWinprop3" testIsWinprop3,
                testCase "testIsWinprop4" testIsWinprop4,
                testCase "testIsWinprop5" testIsWinprop5,
                testCase "testIsWinprop6" testIsWinprop6
            ],
        testGroup "Testing the filterminindexes" [
                testCase "testFilterminindexes1" testFilterminindexes1,
                testCase "testFilterminindexes2" testFilterminindexes2,
                testCase "testFilterminindexes3" testFilterminindexes3,
                testCase "testFilterminindexes4" testFilterminindexes4,
                testCase "testFilterminindexes5" testFilterminindexes5,
                testCase "testFilterminindexes6" testFilterminindexes6,
                testCase "testFilterminindexes7" testFilterminindexes7,
                testCase "testFilterminindexes8" testFilterminindexes8,
                testCase "testFilterminindexes9" testFilterminindexes9,
                testCase "testFilterminindexes10" testFilterminindexes10,
                testCase "testFilterminindexes11" testFilterminindexes11,
                testCase "testFilterminindexes12" testFilterminindexes12,
                testCase "testFilterminindexes13" testFilterminindexes13,
                testCase "testFilterminindexes14" testFilterminindexes14,
                testCase "testFilterminindexes15" testFilterminindexes15,
                testCase "testFilterminindexes16" testFilterminindexes16,
                testCase "testFilterminindexes17" testFilterminindexes17,
                testCase "testFilterminindexes18" testFilterminindexes18,
                testCase "testFilterminindexes19" testFilterminindexes19,
                testCase "testFilterminindexes20" testFilterminindexes20,
                testCase "testFilterminindexes21" testFilterminindexes21,
                testCase "testFilterminindexes22" testFilterminindexes22,
                testCase "testFilterminindexes23" testFilterminindexes23,
                testCase "testFilterminindexes24" testFilterminindexes24,
                testCase "testFilterminindexes25" testFilterminindexes25,
                testCase "testFilterminindexes26" testFilterminindexes26,
                testCase "testFilterminindexes27" testFilterminindexes27,
                testCase "testFilterminindexes28" testFilterminindexes28,
                testCase "testFilterminindexes29" testFilterminindexes29
            ],
        testGroup "Testing the filtermaxindexes" [
                testCase "testFiltermaxindexes1" testFiltermaxindexes1,
                testCase "testFiltermaxindexes2" testFiltermaxindexes2,
                testCase "testFiltermaxindexes3" testFiltermaxindexes3,
                testCase "testFiltermaxindexes4" testFiltermaxindexes4,
                testCase "testFiltermaxindexes5" testFiltermaxindexes5,
                testCase "testFiltermaxindexes6" testFiltermaxindexes6,
                testCase "testFiltermaxindexes7" testFiltermaxindexes7,
                testCase "testFiltermaxindexes8" testFiltermaxindexes8,
                testCase "testFiltermaxindexes9" testFiltermaxindexes9,
                testCase "testFiltermaxindexes10" testFiltermaxindexes10,
                testCase "testFiltermaxindexes11" testFiltermaxindexes11,
                testCase "testFiltermaxindexes12" testFiltermaxindexes12,
                testCase "testFiltermaxindexes13" testFiltermaxindexes13,
                testCase "testFiltermaxindexes14" testFiltermaxindexes14,
                testCase "testFiltermaxindexes15" testFiltermaxindexes15,
                testCase "testFiltermaxindexes16" testFiltermaxindexes16,
                testCase "testFiltermaxindexes17" testFiltermaxindexes17,
                testCase "testFiltermaxindexes18" testFiltermaxindexes18,
                testCase "testFiltermaxindexes19" testFiltermaxindexes19,
                testCase "testFiltermaxindexes20" testFiltermaxindexes20,
                testCase "testFiltermaxindexes21" testFiltermaxindexes21,
                testCase "testFiltermaxindexes22" testFiltermaxindexes22,
                testCase "testFiltermaxindexes23" testFiltermaxindexes23,
                testCase "testFiltermaxindexes24" testFiltermaxindexes24,
                testCase "testFiltermaxindexes25" testFiltermaxindexes25,
                testCase "testFiltermaxindexes26" testFiltermaxindexes26,
                testCase "testFiltermaxindexes27" testFiltermaxindexes27,
                testCase "testFiltermaxindexes28" testFiltermaxindexes28,
                testCase "testFiltermaxindexes29" testFiltermaxindexes29
            ],
       testGroup "Testing election results" [
                testCase "Melbourne 2013" ourelection
            ]

    ]

