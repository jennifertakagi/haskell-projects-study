module ProjectHR where

-- Type "Position" with value constructor "Intern/Developer/Lead/Mamager"
data Position = Intern | Developer | Lead | Manager deriving Show

-- Type "Person" with value constructor "Person{position, name}"
data Person = Person {position :: Position, name :: String} deriving Show

-- Function that receives a Person and return the salary (Double type)
showSalary :: Person -> Double
showSalary (Person Intern _) = 1500
showSalary (Person Developer _) = 5750.15
showSalary (Person Lead _) = 8000
showSalary (Person Manager _) = 10807.20

-- Function that receives a Person and return a message showing: name, position and salary
showPaycheck :: Person -> String
showPaycheck p = "{name: \"" ++ (name p) ++
  "\", position: \"" ++ show (position p) ++
  "\", salary: " ++ show (showSalary p) ++ "}"

-- Function that receives a Person and promote it to next position
promotion :: Person -> Person
promotion (Person Intern n) = Person Developer n
promotion (Person Developer n) = Person Lead n
promotion (Person Lead n) = Person Manager n
promotion (Person _ n) = Person Manager n
