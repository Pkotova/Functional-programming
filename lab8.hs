main :: IO()
main = do

 print(isInteresting 410)
 print(containsSix 1235)
 print(containsSix 12635)
 print(sumNums 60 71)
 print(isProgression [2])
 print(elements[[1,2,7  ],[2,4,6,8],[12,14,16]])
--4k + 1 ---> 0 9 13 17 21 25 29 33 37 41 45 49 53 57 61 65 69 

--1 task
sumDigits :: Int -> Int
sumDigits n
 | n < 10 = n 
 | otherwise = (mod n 10) + sumDigits(div n 10)
 
isInteresting :: Int -> Bool
isInteresting n = mod n (sumDigits n) == 0

--2 task
containsSix :: Int -> Bool
containsSix 0 = False
containsSix n = (mod n 10 == 6) || containsSix(div n 10)

sumNums :: Int -> Int -> Int
sumNums a b = sum [k | k <- [a..b], containsSix k, mod k 4 == 1]

--3 task

isProgression :: [Int] -> Bool
isProgression [_] = True
isProgression (x1:x2:xs) = helper (x2:xs) (x2 - x1)
 where
  helper [_] _        = True
  helper (x2:x3:xs) d = x3 - x2 == d && helper (x3:xs) d

elements :: [[Int]] -> [[Int]]
elements list = [x | x <- list, isProgression x]

--8 task
sumUnique::[[Int]] -> Int
sumUnique list = sum(concat [makeUnique x| x <- list])

makeUnique::[Int] -> [Int]
makeUnique [] = []
makeUnique(x:xs)
 |elem x xs = makeUnique [y | y <- xs, x/= y]
 |otherwise = x : makeUnique xs
