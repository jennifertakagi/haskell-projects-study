module Jokenpo where

data Jokenpo = Rock | Paper | Scissors deriving Show
data Player = Player {jokenpo :: Jokenpo, name :: String} deriving Show

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
