-- Function that get average of a list of double numbers
average :: [Double] -> Double
average ls = (foldl (+) 0.0 ls) / calcAverage
  where calcAverage = fromIntegral $ length ls

-- Function that get the palindromes of a list of string
getPalindrome :: [String] -> [String]
getPalindrome ls = filter (isPalindrome) ls
  where isPalindrome s = reverse s == s

-- Function that get the odd numbers of a list
getOdd :: [Int] -> [Int]
getOdd li = filter (isOdd) li
  where isOdd i = (mod) i 2 /= 0

-- Function that get the even numbers of a list
getEven :: [Int] -> [Int]
getEven li = filter (isEven) li
  where isEven i = (mod) i 2 == 0

-- Function that get the prime numbers of a list
getPrimeNumbers :: [Int] -> [Int]
getPrimeNumbers li = filter (\x -> getNotPrime x == 0) li
  where getNotPrime i = length $ [n | n <- [2, 3, 5, 7], i /= n, (mod) i n == 0]

-- Function that remove the numbers multiply by four and double the value
calcDoubleRemoveFourMultiply :: [Int] -> [Int]
calcDoubleRemoveFourMultiply li = filter (isNotMultiplyFour) $ map (*2) li 
  where isNotMultiplyFour i = (mod) i 4 /= 0
