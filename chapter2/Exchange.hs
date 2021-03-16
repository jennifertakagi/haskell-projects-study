module Exchange where

-- Type "Exchange" with value constructor "Euro/Real/Dollar"
data Exchange = Dollar | Euro | Real deriving Show

-- Type "Money" with value constructor "Money{value, cur}"
data Money = Money {value :: Double, cur :: Exchange} deriving Show

-- Function that receives a Money and convert it to Dollar unit
convertDollar :: Money -> Double
convertDollar (Money val Dollar) = val
convertDollar (Money val Euro) = val * 0.84
convertDollar (Money val Real) = val * 5.58

-- Function that receives a Money and convert it to Euro unit
convertEuro :: Money -> Double
convertEuro (Money val Euro) = val
convertEuro (Money val Dollar) = val * 1.19
convertEuro (Money val Real) = val * 6.63

-- Function that receives a Money and convert it to Real unit
convertReal :: Money -> Double
convertReal (Money val Real) = val
convertReal (Money val Dollar) = val * 0.18
convertReal (Money val Euro) = val * 0.15
