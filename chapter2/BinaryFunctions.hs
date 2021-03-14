module BinaryFunctions where

-- Type "Binary" with value constructor "0/1"
data Binary = Zero | One deriving Show

-- Type "Function" with value constructor "Sum2/Greater/Less/Multiply2"
data Function = Sum2 | Greater | Less | Multiply2 deriving Show

-- Function that receives a Function and two Binary, make the operation and returns a Binary
apply :: Function -> Binary -> Binary -> Binary
apply Sum2 Zero Zero = Zero
apply Sum2 Zero One = One
apply Sum2 One One = One

apply Greater Zero Zero = Zero
apply Greater Zero One = One
apply Greater One One = One

apply Less Zero Zero = Zero
apply Less Zero One = Zero
apply Less One One = One

apply Multiply2 Zero Zero = Zero
apply Multiply2 Zero One = Zero
apply Multiply2 One One = One