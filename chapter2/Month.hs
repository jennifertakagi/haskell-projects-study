module Month where

-- Type "Month" with value constructor all the months
data Month = January | February | March | April | May | June | July | August
  | September | October | November | December deriving Show
data Season = Winter | Spring | Summer | Fall deriving Show

getDaysMonth :: Month -> Int
getDaysMonth February = 28
getDaysMonth April = 30
getDaysMonth June = 30
getDaysMonth September = 30
getDaysMonth November = 30
getDaysMonth _ = 31

getNextMonth :: Month -> Month
getNextMonth January = February
getNextMonth February = March
getNextMonth March = April
getNextMonth April = May
getNextMonth May = June
getNextMonth June = July
getNextMonth July = August
getNextMonth August = September
getNextMonth September = October
getNextMonth October = November
getNextMonth November = December
getNextMonth December = January

getSeason :: Month -> Season
getSeason January = Winter
getSeason February = Winter
getSeason March = Spring
getSeason April = Spring
getSeason May = Spring
getSeason June = Summer
getSeason July = Summer
getSeason August = Summer
getSeason September = Fall
getSeason October = Fall
getSeason November = Fall
getSeason December = Winter
