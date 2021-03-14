module Question where

-- Type "Question" with value constructor "Yes/No"
data Question = Yes | No deriving Show

-- Function that receives a Question and return 1/0 to Yes/No
questionNum :: Question -> Int
questionNum Yes = 1
questionNum No = 0

-- Function that receives a list of Question and return 1/0 to Yes/No to each one
listQuestions :: [Question] -> [Int]
listQuestions questions = [ questionNum q | q <- questions]
