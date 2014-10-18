module Candidate where

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
import PrefStringUtil

-- | This defines the data structure Candidate which represents candidates and their parties.
data Candidate = Candidate
    { name :: String,
    party :: String } deriving (Eq, Show, Read)

-- | There's a "Show" method provided like the Haskell's equivalent of Python's __repr__. So we
-- provide candstrout - our equivalent of __str__ in Python. It basically prints name and
-- a delimiter and a party.
candstrout :: Candidate -> Char -> String
candstrout candval delim = name candval ++ [delim] ++ party candval

-- | The candstrin method is the opposite of candstrout. Given a string of the form "name delim
-- party", it constructs a Candidiate from there.
candstrin :: String -> Char -> Maybe Candidate
candstrin "" _ = Nothing
candstrin stringval delim
    | length splitpiece == 2 = Just Candidate{name = head splitpiece, party= splitpiece !! 1}
    | otherwise   = Nothing
    where splitpiece = stripandtrim stringval delim

-- | The createcandidatesfromline splits up a line into candidates by the first delimiter, and
-- then splits up candidates into name and party via the second delimiter; hopefully this
-- creates Candidate instances.
createcandidatesfromline :: String -> Char -> Char -> [Maybe Candidate]
createcandidatesfromline stringval intercanddelim intracanddelim =
    [candstrin x intracanddelim | x <- stripandtrim stringval intercanddelim]

