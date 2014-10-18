module PrefStringUtil where

import Data.List
import Data.List.Split
import Data.Char (isSpace)
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
