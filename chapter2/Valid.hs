module Valid where

-- Type "Valid" with value constructor "Yes String/No"
data Valid = Yes String | No deriving Show

-- Function that receives a String, return "Yes String" if is a valid one,
-- return "No" if it's a empty string
isValidName :: String -> Valid
isValidName "" = No
isValidName name = Yes name
