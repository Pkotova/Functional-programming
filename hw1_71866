import Data.Char              

main :: IO()
main = do

 print (solveQuadratic 1 4 4 ) -- det = 0, x1 = x2 = -2                              *** return (-2,-2)
 print (sumPrimes 10 3 ) -- 11 + 13 + 17 = 41                                        *** return 41
 print (countPalindromes 10 25) -- 11, 22 are the only palindromes in the range      *** return 2
 print (truncatablePrime 3797) -- 3797 ⇢ True ( 3797, 379, 37 and 3 are primes)      *** return true
                               --but 47 is not trundarable because 4 is not prime

 
-- Checking if the given number is prime
isPrime :: Integer -> Bool
isPrime 1 = False 
isPrime number = helper 2
 where
  helper counter 
   | counter * counter > number = True
   | (number `mod` counter) == 0 = False
   | otherwise              = helper (counter + 1)
  
------------------------------------------
 -- 1 задача
------------------------------------------
 
solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic a b c  = if d < 0 then error "0" else (x1, x2)
-- cheching the value of discriminant, if there is solution in R
 where
-- calculating roots
  x1 = (- b + sqrt d ) / 2 * a
  x2 = (- b - sqrt d ) / 2 * a
  d = b * b - 4 * a * c
  
-------------------------------------------
 -- 2 задача 
-------------------------------------------
sumPrimes :: Integer -> Integer -> Integer
sumPrimes n k
 | k <= 0    = 0
 | isPrime n = n + sumPrimes (n +1) (k-1)
 | otherwise = 0 + sumPrimes (n + 1) k
-------------------------------------------
 -- 3 задача
-------------------------------------------
-- checking if the given number is a palindrome f.e. 123321 is a palindrome, but 1345 is not a palindrome
isPalindrome::Integer -> Bool
isPalindrome number = number == reversedNumber number
-- check if the number is equal to its reversed
-- 12321 - reversed...12321 
reversedNumber :: Integer -> Integer
reversedNumber number = helper number 0
 where 
 helper rest acc
  | rest < 10 = rest + acc * 10
  | otherwise = helper (rest `div` 10) ((rest `mod` 10) + acc * 10)

countPalindromes :: Integer -> Integer -> Integer
countPalindromes a b = fromIntegral(length[x | x <- [a..b], isPalindrome x]) -- given interval

-------------------------------------------
--4 задача 
-------------------------------------------

truncatablePrime :: Integer -> Bool
truncatablePrime number 
 | number == 0              = True
 | isPrime number == False  = False -- using the function isPrime 
 | otherwise           = True && (truncatablePrime (div number 10)) 

 
