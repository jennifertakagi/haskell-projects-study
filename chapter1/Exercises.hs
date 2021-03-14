module Exercises where

-- Function that verifies if the String length is odd or even: return a Bool
checkStringLength :: String -> Bool
checkStringLength stringer = (odd . length) stringer

-- Function that receives a list of string and returns
-- a list with all strings reversed
reverseStrings :: [String] -> [String]
reverseStrings stringers = [(reverse) s | s <- stringers]

-- Function that receives a list of string and returns
-- a list with each string length, excluding the even length
getOddLength :: [String] -> [Int]
getOddLength stringers = [(length) s | s <- stringers, (odd . length) s]

-- Function head with a composition of two functions
getHeadElement :: [a] -> [a]
getHeadElement listParam = (last . reverse) listParam

-- Function that receives a integer and return a tuple with 4 coordinates,
-- each element multiplied by index adding 2
multiplyInt :: Int -> (Int)
multiplyInt number = (number * 2, number * 3, number * 4, number * 5)