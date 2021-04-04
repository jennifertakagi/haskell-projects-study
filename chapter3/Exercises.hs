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

-- Type "Day" with value constructor "Sunday/Monday/Tuesday/Wednesday/Thursday/Friday/Saturday"
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving Show

-- Function that get all Tuesday from a list of Day
filterTuesday :: [Day] -> [Day]
filterTuesday days = filter (isTuesday) days
  where isTuesday Tuesday = True
        isTuesday _ = False

-- Type "Exchange" with value constructor "Euro/Real/Dollar"
data Exchange = Dollar | Real deriving Show

-- Type "Money" with value constructor "Money{value, cur}"
data Money = Money {value :: Double, cur :: Exchange} deriving Show

-- Function that convert currency to Dollar or Money
convertCurrency :: [Money] -> [Double]
convertCurrency list = map (exchangeMoney) list

exchangeMoney :: Money -> Double
exchangeMoney (Money val Dollar) = val * 5.71
exchangeMoney (Money val Real) = val * 0.18

-- Function that get all the Dollar from a list
filterDollar :: [Money] -> [Money]
filterDollar list = filter (isDollar) list
  where isDollar (Money _ Dollar) = True
        isDollar (Money _ _) = False

-- Function that sum the value of Dollar on list
sumDollar :: [Money] -> Double
sumDollar list = (foldl isDollar 0.0) $ filterDollar list
  where isDollar acc (Money val Dollar) = acc + val

-- Function that count the quantity of Dollar on list
countDollar :: [Money] -> Int
countDollar list = length $ filterDollar list
