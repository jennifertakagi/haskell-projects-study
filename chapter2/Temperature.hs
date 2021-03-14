module Temperature where

-- Type "Temperature" with value constructor "Celsius/Fahrenheit/Kelvin"
data Temperature = Celsius | Fahrenheit | Kelvin deriving Show

-- Function that receives a value and a Temperature and return it converted to Celsius
convertCelsius :: Double -> Temperature -> Double
convertCelsius value Fahrenheit = (value - 32) * 5/9
convertCelsius value Kelvin = value - 273.15
convertCelsius value _ = value

-- Function that receives a value and a Temperature and return it converted to Kelvin
convertKelvin :: Double -> Temperature -> Double
convertKelvin value Fahrenheit = (value - 32) * 5/9 + 273.15
convertKelvin value Celsius = value + 273.15
convertKelvin value _ = value

-- Function that receives a value and a Temperature and return it converted to Fahrenheit
convertFahrenheit :: Double -> Temperature -> Double
convertFahrenheit value Kelvin = (value - 273.15) * 9/5 + 32
convertFahrenheit value Celsius = (value * 9/5) + 32
convertFahrenheit value _ = value
