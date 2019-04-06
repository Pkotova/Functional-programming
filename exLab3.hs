 main :: IO()
 main = do

 --print(isAscending 12345)
 print(isPrime 7)
 print(isPerfect 6)
 print(isPerfect 18)
 print(primesInRange 1 10)
 print(isPrime 5)
 print(reverseNumber 10393)
 
isAscending :: Int -> Bool
isAscending n
  | n < 10 = True
  | mod (div n 10) 10 < mod n 10  = isAscending (div n 10) 
  | otherwise = False
  
isPrime :: Int -> Bool
isPrime 1  =  False
isPrime n  = (length [x | x <- [2..n-1], n `mod` x == 0]) == 0

isPerfect :: Int ->  
isPerfet 1 = False
isPerfect n = n == sumDevidors[x | x <- [1..n-1], mod n x == 0]
 where 
  sumDevidors :: [Int] -> Int
  sumDevidors [] = 0
  sumDevidors (x:xs) = x + sumDevidors xs
  
primesInRange :: Int -> Int -> [Int]
primesInRange a b = [x | x <- [a..b], isPrime x]


reverseNumber :: Integer -> Integer
reverseNumber n = helper n 0
 where
  helper rest acc
   | rest < 10 = rest + acc * 10
   | otherwise = helper (rest `div` 10) ((rest `mod` 10) + acc * 10)