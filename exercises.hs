main :: IO()
main = do

 print(inside 1 5 2)
 print(sumSqrts 3 4)
 print(myFact 5)
 print(myFactoriel 5)
 print(myFib 3)
 print(myGcd 4 2)
 
inside :: Int -> Int -> Int -> Bool
inside a b x = (x >= a) && (x <= b)

sumSqrts :: Int -> Int -> Int
sumSqrts a b = sQuart a + sQuart b
 where
 sQuart x = x * x
 
myFact :: Int -> Int
myFact 0 = 1
myFact n =  n * myFact(n-1)

myFactoriel :: Integer -> Integer
myFactoriel n = myFactIter n 1
 where
  myFactIter 0 acc = acc
  myFactIter n acc = myFactIter (n - 1) (acc * n)

myFib :: Int -> Integer
myFib  0 = 0
myFib  1 = 1
myFib  n = myFib (n - 1) + myFib (n - 2)

fibIter :: Integer -> Integer
fibIter n = fibHelper n 1 0 0
fibHelper :: Integer -> Integer -> Integer -> Integer -> Integer
fibHelper n res pre i =
 if i <= n then fibHelper n (res + pre) res (i + 1) else res
 
myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b)
