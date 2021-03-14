module Length where

-- Type "Length" with value constructor "Inch/Yard/Foot"
data Length = Inch | Yard | Foot deriving Show
data LengthUnit = LengthUnit{value :: Double, name:: Length} deriving Show

-- Function that receives a LengthUnit, transform it in meter unit, and return a Double
convertMeters :: LengthUnit -> Double
convertMeters (LengthUnit value Inch) = value * 0.0254
convertMeters (LengthUnit value Yard) = value * 0.9144
convertMeters (LengthUnit value Foot) = value * 0.3048

-- Function that receives a LengthUnit, transform it in imperial unit, and return a Double
convertImperial :: LengthUnit -> Double
convertImperial (LengthUnit value Inch) = value / 0.0254
convertImperial (LengthUnit value Yard) = value / 0.9144
convertImperial (LengthUnit value Foot) = value / 0.3048
