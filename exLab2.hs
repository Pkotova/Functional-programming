main :: IO()
main = do
 print(myMin 4 5)
 print(countDigits 1234)
 print(countDig 12321)
 print(sumDigits 123)
 print(sumDigitsItter 2039)
 
myMin :: Int -> Int ->Int
myMin a b = if(a > b) then b else a

countDigits :: Int -> Int
countDigits n 
 | n < 10 = 1
 |otherwise = 1 + countDigits ( n `div` 10)

countDig :: Int -> Int
countDig n = countDigItter n 0
 where 
 countDigItter n digits 
  | n < 10 = digits + 1
  | otherwise = countDigItter (n `div` 10) (digits + 1)
  

sumDigits :: Int -> Int
sumDigits n 
 | n < 10 = n
 | otherwise = (mod n 10) + sumDigits(div n 10)
 
sumDigitsItter :: Int -> Int
sumDigitsItter n = sumDig n 0 
 where
 sumDig n sum
  |  n < 10    =  n 
  | otherwise = sumDig(n `div` 10) (sum + (n `mod` 10))
  
  
