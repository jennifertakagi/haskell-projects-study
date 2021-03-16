module Crypto where

-- Type "Crypto" with value constructor "Message/Cyphered/Error"
data Crypto = Message String | Cyphered String | Error deriving Show

encrypt :: Crypto -> Crypto
encrypt (Message message) = Cyphered [succ m | m <- message]
encrypt (Cyphered message) = Error

decrypt :: Crypto -> Crypto
decrypt (Cyphered message) = Message [pred m | m <- message]
decrypt (Message message) = Error

encryptAll :: [Crypto] -> [Crypto]
encryptAll listMessage = [encrypt m | m <- listMessage]
