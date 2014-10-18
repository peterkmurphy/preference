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
import OurTests.TestStuff

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

