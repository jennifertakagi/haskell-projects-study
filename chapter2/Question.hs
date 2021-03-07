module Question where

-- Type "Question" with value constructor "Yes/No"
data Question = Yes | No deriving Show

questionNum :: Question -> Int
questionNum Yes = 1
questionNum No = 0

listQuestions :: [Question] -> [Int]
listQuestions questions = [ questionNum q | q <- questions]
