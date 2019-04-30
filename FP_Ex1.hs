main :: IO()
main = do
 print "Hello World"
 print (inside 2 10 7)
 print (squareSums 5 10)
 print (factorial 5)

-- zad 1
inside :: Int -> Int -> Int -> Bool
inside x y z = z >= x && z <= y

--zad 2

-- square :: Int -> Int
-- square a = a * a

squareSums :: Int -> Int -> Int
squareSums a b = (square a) + (square b)
 where 
  square x = x * x

-- zad 3
--zad 3
factorial :: Int -> Int
factorial 0 = 1
factorial a = a * factorial(a - 1)

main  IO()
main = do
 print (5 + 6)            -- 11
 print (div 18 7)         -- 2
 print (18 `div` 7)       -- 2
 print (mod 18 7)         -- 4
 print (18 `mod` 7)       -- 4
 
 print (11 ^ 2)           -- 121
 print (2  0.5)         -- 1.4142..

 print (inside 1 5 3)     -- True
 print (inside 1 5 1)     -- True
 --print (inside 1 5 1.5) -- няма да се компилира, защото третият аргумент не е от тип Int
 
 print (sumSquares 10 15) -- 325
 print (sumSquares' 10 0) -- 100
 print (sumSquares' 10 9) -- 181
 print (sumSquares'' 8 6) -- 100
 print (averageSum 10 15) -- 162.5
 
 print (myFact 5)         -- 120
 print (myFact' 3)        -- 6
 
 -- Примерни тестове за 4-та и 5-та задача
 --print (myFib 4)          -- 5
 --print (myFib' 3)         -- 3
 
 --print (myGcd 13 17)      -- 1
 --print (myGcd 26 39)      -- 13
 
 
-- Задача 1.
inside  Int - Int - Int - Bool
inside a b x = (a = x) && (x = b)
 
 
 
-- Задача 2.

-- първи вариант
sumSquares  Int - Int - Int
sumSquares x y = x  x + y  y

-- втори вариант, чрез образци
sumSquares'  Int - Int - Int
sumSquares' x 0 = x  x
sumSquares' 0 y = y  y
sumSquares' x y = x  x + y  y

-- трети вариант, чрез вложена дефиния на функция
sumSquares''  Int - Int - Int
sumSquares'' x y = square x + square y
 where -- вложена дефииция на функция
  square z = z  z
  
  
-- примерна задача
averageSum  Int - Int - Double
averageSum x y = fromIntegral(sumSquares x y)  2

-- fromIntegral takes Int and returns a Double, so that the  could work



-- Задача 3.

-- рекурсивно решение
myFact  Integer - Integer
myFact 0 = 1                   -- дъно
myFact n = n  myFact (n - 1)


-- итеративно решение, използващо вложена дефиниция
myFact'  Integer - Integer
myFact' n = myFactIter n 1
 where
  myFactIter 0 acc = acc
  myFactIter n acc = myFactIter (n - 1) (acc  n)


