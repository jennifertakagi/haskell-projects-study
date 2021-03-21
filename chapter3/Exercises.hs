average :: [Double] -> Double
average ls = (foldl (+) 0.0 ls) / calcAverage
  where calcAverage = fromIntegral $ length ls

getPalindrome :: [String] -> [String]
getPalindrome ls = filter (isPalindrome) ls
  where isPalindrome s = reverse s == s

getOdd :: [Int] -> [Int]
getOdd li = filter (isOdd) li
  where isOdd i = (mod) i 2 /= 0

getEven :: [Int] -> [Int]
getEven li = filter (isEven) li
  where isEven i = (mod) i 2 == 0

calcDoubleRemoveFourMultiply :: [Int] -> [Int]
calcDoubleRemoveFourMultiply li = filter (isNotMultiplyFour) $ map (*2) li 
  where isNotMultiplyFour i = (mod) i 4 /= 0
