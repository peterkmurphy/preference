module PrefStringUtil where

import Data.List
import Data.List.Split
import Data.Char (isSpace)
import Test.QuickCheck
import Test.QuickCheck.All (quickCheckAll)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe

-- | This is a quick and dirty "Trim whitespace from both sides" function", that is taken straight outta
-- <http://stackoverflow.com/questions/6270324/in-haskell-how-do-you-trim-whitespace-from-the-beginning-and-end-of-a-string Stack Overflow>.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | This takes a string of form "Value1 delim Value2 delim ... Valuen", with a character representing
-- a delimiter. It returns the sequence [Value1, Value2, ... Valuen]. All whitespace is trimmed
-- from all the values.
stripandtrim :: String -> Char -> [String]
stripandtrim stringval charval = [trim x | x <- splitOn [charval] stringval]

-- | This method takes a string, and attempts to parse it into a number.
convertstringtoint :: String -> Maybe Int
convertstringtoint stringval
   | null conversion = Nothing
   | otherwise = Just (fst (head conversion))
   where conversion = reads stringval

-- | This method takes a string of the form "a, b, c, ..." where a, b, c
-- and so on are integer; it attempts to create a sequence of integers
-- from them.

convertstringtoseq :: String -> Maybe [Int]
convertstringtoseq stringval
    | null result = Nothing
    | elem Nothing result = Nothing
    | otherwise = Just [fromJust d | d <- result]
    where result = [convertstringtoint c | c <- stripandtrim stringval ',']

-- This is for test code. This is a constant for a selection of space characters

spacerange = " \t\n\r\f\v\160"

-- | This is used for the existence of whitespace in a string. This is used for testing.
iswhitespacewithin :: String -> Bool
iswhitespacewithin stringval
    | isNothing (find isSpace stringval) = False
    | otherwise = True

-- We need to test iswhitespacewithin, because we use it in other testing code.

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

