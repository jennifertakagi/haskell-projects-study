module Exercises where

-- Function that receives a string and returns all vowels
getVowels :: String -> [Char]
getVowels word = [ w | w <- word, (isVowel) w]

isVowel :: Char -> Bool
isVowel letter = elem letter "aeiouAEIOU"

-- Function that receives a string and checks if it is a palindrome
isPalindrome :: String -> Bool
isPalindrome word = reverse word == word