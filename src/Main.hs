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
            ]
    ]


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

