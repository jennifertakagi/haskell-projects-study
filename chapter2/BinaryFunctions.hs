module BinaryFunctions where

-- Type "Binary" with value constructor "0/1"
data Binary = Zero | One deriving Show

-- Type "Function" with value constructor "Sum2/Greater/Less/Multiply2"
data Function = Sum2 | Greater | Less | Multiply2 deriving Show

-- Function that receives a Function and two Binary, make the operation and returns a Binary
apply :: Function -> Binary -> Binary -> Binary
apply Sum2 Zero Zero = Zero
apply Sum2 Zero One = One
apply Sum2 One One = Zero
apply Sum2 One Zero = One

apply Greater Zero Zero = Zero
apply Greater Zero One = One
apply Greater One One = One
apply Greater One Zero = One

apply Less Zero Zero = Zero
apply Less Zero One = Zero
apply Less One One = One
apply Less One Zero = Zero

apply Multiply2 Zero Zero = Zero
apply Multiply2 Zero One = Zero
apply Multiply2 One One = One
apply Multiply2 One Zero = Zero

-- Function that receives a list of Binary, call Sum2 to each one with "One" and
-- return a list of Int with the Binary converted to integer
binList :: [Binary] -> [Int]
binList binaries = [ (convertInt . apply Sum2 One) b | b <- binaries]

convertInt :: Binary -> Int
convertInt Zero = 0
convertInt One = 1