import Data.List
import Data.Char
main :: IO()
main = do 

 print (biggestNumber [1,2,3,4,5])
 print (biggestNumber [1,5,5,3,5])
 print (intersectionPoints (\x -> x) (\x -> x * x) (-5) 5)
 print (intersectionPoints (\x -> x) (\x -> x * x + 1) (-5) 5)
 print (iterator [1,2,3] (+1))

 
-- TASK 01
biggestNumber:: [Int] -> Int             
biggestNumber l = numberCreator (sorting l) 0 -- creating the number by sorted list
 where 
  numberCreator (x:xs) num
    |xs == []  = num * 10 + x
    |otherwise = numberCreator (xs) (num *10 + x) -- creating the number 
    
sorting:: [Int] -> [Int]       -- dec sorting
sorting []     = []
sorting (x:xs) = sorting big ++ [x] ++ sorting smal
 where 
  smal = filter (<=x) xs 
  big  = filter (>x)  xs
  
-- TASK 02
intersectionPoints :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> [Int]
intersectionPoints f g a b = [x | x <- [a..b], (f x) == (g x)]  -- f and g has the same value 

-- TASK 03
iterator :: [Int] -> (Int -> Int) -> Bool
iterator l f = length[s | s <- l, (f s) == (tail l !! head i)] == ((length l) - 2)
 where i = [ m | m <- [1..(length l)]]  -- 
 
-- TASK 04
data BTree = Empty | BTree Int BTree BTree
-- left tree and right tree 
levelsum :: BTree -> Int -> Int
levelsum Empty _                   = 0
levelsum (BTree n leftT rightT) 1  = n
levelsum (BTree _ leftT rightT) k  = levelsum leftT (k - 1) +  levelsum rightT ( k - 1)


