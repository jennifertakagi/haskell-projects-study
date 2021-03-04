module ProjectHR where

data Position = Intern | Developer | Lead | Manager deriving Show
data Person = Person {position :: Position, name :: String} deriving Show

showSalary :: Person -> Double
showSalary (Person Intern _) = 1500
showSalary (Person Developer _) = 5750.15
showSalary (Person Lead _) = 8000
showSalary (Person Manager _) = 10807.20

showPaycheck :: Person -> String
showPaycheck p = "{name: \"" ++ (name p) ++
  "\", position: \"" ++ show (position p) ++
  "\", salary: " ++ show (showSalary p) ++ "}"

promotion :: Person -> Person
promotion (Person Intern n) = Person Developer n
promotion (Person Developer n) = Person Lead n
promotion (Person Lead n) = Person Manager n
promotion (Person _ n) = Person Manager n
