module Crypto where

-- Type "Crypto" with value constructor "Message/Cyphered/Error"
data Crypto = Message String | Cyphered String | Error deriving Show

-- Function that receives a Crypto and encrypt the Message
encrypt :: Crypto -> Crypto
encrypt (Message message) = Cyphered [succ m | m <- message]
encrypt (Cyphered message) = Error

-- Function that receives a Crypto and decrypt the Cyphered
decrypt :: Crypto -> Crypto
decrypt (Cyphered message) = Message [pred m | m <- message]
decrypt (Message message) = Error

-- Function that receives a list of Crypto and encrypt them
encryptAll :: [Crypto] -> [Crypto]
encryptAll listMessage = [encrypt m | m <- listMessage]
