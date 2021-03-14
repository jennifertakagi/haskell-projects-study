module Jokenpo where

-- Type "Jokenpo" with value constructor "Rock/Paper/Scissors"
data Jokenpo = Rock | Paper | Scissors deriving Show

-- Type "Person" with value constructor "Player{jokenpo, name}"
data Player = Player {jokenpo :: Jokenpo, name :: String} deriving Show

-- Function that receives two Players and return the winner
playJokenpo :: Player -> Player -> String
playJokenpo (Player Rock n1) (Player Rock n2) = "It's a tie!"
playJokenpo (Player Rock n1) (Player Paper n2) = "Winner is " ++ n2
playJokenpo (Player Rock n1) (Player Scissors n2) = "Winner is " ++ n1

playJokenpo (Player Paper n1) (Player Paper n2) = "It's a tie!"
playJokenpo (Player Paper n1) (Player Rock n2) = "Winner is " ++ n1
playJokenpo (Player Paper n1) (Player Scissors n2) = "Winner is " ++ n2

playJokenpo (Player Scissors n1) (Player Scissors n2) = "It's a tie!"
playJokenpo (Player Scissors n1) (Player Rock n2) = "Winner is " ++ n2
playJokenpo (Player Scissors n1) (Player Paper n2) = "Winner is " ++ n1
