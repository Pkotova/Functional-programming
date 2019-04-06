main :: IO()
main = do
 
 print(isInteresting 400)
 print(containsSix 23445344)
 print(sumNums 60 70)
 print(isProgression [1,4,6,8,10])
 
sumDigits :: Int -> Int
sumDigits n
 | n < 10 = n 
 | otherwise = (mod n 10) + sumDigits(div n 10)
 
isInteresting :: Int -> Bool
isInteresting n = mod n (sumDigits n) == 0


containsSix :: Int -> Bool
containsSix 0 = False
containsSix n = (mod n 10 == 6) || containsSix(div n 10)

sumNums :: Int -> Int -> Int
sumNums a b = sum [n | n <- [a..b], containsSix n, mod (n - 1) 4 == 0]

isProgression :: [Int] -> Bool
isProgression [_] = False
isProg (x1:x2:xs) = helper (x2:xs) (x2 - x1)
 where
  helper [_] _        = True
  helper (x2:x3:xs) d = x3 - x2 == d && helper (x3:xs) d

elements :: [[Int]] -> [[Int]]
elements list = [x | x <- list, isProg x]