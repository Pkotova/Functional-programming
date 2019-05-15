import Data.List
import Data.Char
main :: IO()
main = do
 
  print (firstName p1)
  print (city p2)
  
data Person = Person1 {firstName :: String, lastName :: String, age :: Int} | 
             Person2 {name :: String, city :: String} deriving Show
p1 :: Person 
p1 = Person1 "Petya" "Kotova" 20

p2 :: Person
p2 = Person2 "Petya" "Plovdiv"
