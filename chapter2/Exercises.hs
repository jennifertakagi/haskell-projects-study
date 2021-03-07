module Exercises where

-- Function that receives a string and returns all vowels
getVowels :: String -> [Char]
getVowels word = [ w | w <- word, (isVowel) w]

isVowel :: Char -> Bool
isVowel letter = elem letter "aeiouAEIOU"
