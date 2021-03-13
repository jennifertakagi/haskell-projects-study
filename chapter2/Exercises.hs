module Exercises where

-- Function that receives a string and returns all vowels
getVowels :: String -> [Char]
getVowels word = [ w | w <- word, (isVowel) w]

isVowel :: Char -> Bool
isVowel letter = elem letter "aeiouAEIOU"

-- Function that receives a string and checks if it is a palindrome
isPalindrome :: String -> Bool
isPalindrome word = reverse word == word

-- Function that receives a integer list and remove the numbers: odd, even
-- multiples by 7 and negative ones. The return is reverse list
cleanIntList :: [Int] -> [Int]
cleanIntList intList = (reverse) [ i | i <- intList, (odd) i, (oddNotMultiplySeven) i, (signum) i == 1]

oddNotMultiplySeven :: Int -> Bool
oddNotMultiplySeven value = (odd) value && (mod) value 7 /= 0
