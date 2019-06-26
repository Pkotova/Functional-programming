import Data.List
import Data.Char
main :: IO()
main = do

 --print (sumDigits 410)
 --print (isInteresting 410)
 print (containsSix 12554)
 print (sumNumbers 60 70)
 print (isProgression [2,4,6])
 print (progressions [[2,4,6],[2,2,4]])
 print (progressions [[2,2,4]])
 print ("tralalla")
 print (reversedMaxSuf 563428)
 print (sumUnique [[1,2,3,2],[-4,-4],[5]])
 print (isSpecial 91)
 print (isSpecial 81)
 print (getSpecials 2 100)
 print (specials 1  100)
 print (isPerfect 8)


--squares
--factoriel
--countDigits
--isAscending/Descening
--fibonacii
--isPrime
--isPerfect
--Primes in range
--Perfects in range
--chunks of list into n-elements lists
--normalize to capital
-- make upper, makeLower, isCapital, isNumber
--prodDivSum
-- mygcd
--reverse number 
--isSpecial

{--task 01
sumDigits :: Integer -> Integer
sumDigits n = 0
sumDigits n = mod n 10 + sumDigits(div n 10)

isInteresting :: Integer -> Bool
isInteresting n = mod n (sumDigits n) == 0
-}
--task 02
containsSix :: Integer -> Bool
containsSix 0 = False
containsSix n = (mod n 10 == 6) || containsSix(div n 10)

sumNumbers :: Integer -> Integer -> Integer
sumNumbers a b = sum[ x | x <- [a..b], containsSix x, mod x 4 == 1]

--task 03
isProgression :: [Int] -> Bool
isProgression []  = False
isProgression [_] = True
isProgression(x1:x2:xs) = helper (x2:xs) (x2 - x1)
  where
   helper [_] _        = True
   helper (x2:x3:xs) d = x3 - x2 == d && helper (x3:xs) d
   
progressions :: [[Int]] -> [[Int]]
progressions list = [x | x <-list, isProgression x]

--task 07
reversedMaxSuf :: Int -> Int
reversedMaxSuf n = helper n 0
 where 
   helper number result = 
    if mod result 10 >= mod number 10 then result
    else helper (div number  10) (result * 10 + mod number 10)

--task 08
sumUnique :: [[Int]] -> Int
sumUnique lst = sum (concat [getUnique x | x <- lst])

getUnique :: [Int] -> [Int]
getUnique [] = []
getUnique (x:xs)
 | elem x xs = getUnique [y | y <- xs, x /= y]
 | otherwise = x : getUnique xs

--more tasks 

--squares

squares :: Double -> Double -> Double -> [(Double, Double)]
squares a b h = [(x, x * x) | x <- [a, a + h .. b]]

--fubonaci
fibRec :: Integer -> Integer
fibRec 0 = 1
fibRec 1 = 1
fibRec n = fibRec (n - 2) + fibRec (n - 1)

--factoriel
myFact :: Integer -> Integer
myFact 0 = 1                   -- дъно
myFact n = n * myFact (n - 1)

--count digits 
countDigits :: Int -> Int
countDigits n
 |n < 10 = 1
 |otherwise = 1 + countDigits (div n 10)
 
--isAscending / Descening
isAscending :: Integer -> Bool
isAscending n
 | n < 10                                 = True
 | ((n `div` 10) `mod` 10) < (n `mod` 10) = isAscending (n `div` 10)
 | otherwise                              = False
 
--isPrime
isPrime :: Integer -> Bool
isPrime 1  =  False
isPrime n  = (length[x | x <- [2..n-1], (mod n x) == 0]) == 0

--isPerfect
isPerfect :: Integer -> Bool
isPerfect n = n == sum[x | x <- [1..n - 1], n `mod` x == 0]

 
 --primes in range
primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [x | x <- [a..b], isPrime x] 

--perfects in range
perfectsInRange :: Integer -> Integer -> [Integer]
perfectsInRange a b = [x | x <- [a..b], isPerfect x]

--chuncks of 
chunksOf :: Int -> [a] -> [[a]]
chunksOf size lst
 | length lst <= size = [lst]
 | otherwise          = (take size lst) : (chunksOf size (drop size lst))
 
--normalize to capital
normalizeMsg :: String -> String
normalizeMsg  [] = []
normalizeMsg (x:xs)
 |isDigit x = error "Digits not allowed!"
 |isLower x = toUpper x : normalizeMsg xs
 |isUpper x = x : normalizeMsg xs
 |otherwise = normalizeMsg xs
 
-- string functions 
makeUpper :: Char -> Char
makeUpper ch = chr (ord ch + offset)
 where
  offset = ord 'A' - ord 'a'

isLowerLetter :: Char -> Bool
isLowerLetter ch = ch >= 'a' && ch <= 'z'

isCapital :: Char -> Bool
isCapital ch = ch >= 'A' && ch <= 'Z'
 
isNum :: Char -> Bool
isNum num = num >= '0' && num <= '9'

--prodDivSum
prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv numbers k = product [number | number <- numbers,
 sum [divisor | divisor <- [1..number], number `mod` divisor == 0] `mod` k == 0]
 
--gcd
myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b)
 
--reversenumber
reverseNumber :: Integer -> Integer
reverseNumber n = helper n 0
 where
  helper rest acc
   | rest < 10 = rest + acc * 10
   | otherwise = helper (rest `div` 10) ((rest `mod` 10) + acc * 10)
   
--isSpecial reversed 
isSpecial :: Integer -> Bool
isSpecial 1 = False
isSpecial result = isPrime(reverseNumber result)

special :: Integer -> Bool
special 1 = False
--special result = isPrime(sumDigits result)

specials :: Integer -> Integer ->[Integer]
specials a b = [x | x<-[a..b], special x]

getSpecials :: Integer ->Integer ->[Integer]
getSpecials a b = [x | x<-[a..b], isSpecial x]
 
 -- Задача 1. Да се дефинира функция coPrime, която проверява
-- дали две числа са взаимно прости.
coPrime :: Int -> Int -> Bool
coPrime a b = length[ p | p <- [2..b], (mod a p == 0) && (mod b p == 0)] == 0 

-- (coPrime 2 3) -> True
-- (coPrime 10 15) -> False

-- Задача 2. Да се напише функция phiN, която пресмята
-- функцията на Ойлер за подадено естествено число.
-- Функцията (phiN n) е дефинирана по следния начин:
-- phiN 1 = 1
-- phiN n = броят на числата по-малки от n и взаимно прости с n

phiN :: Int -> [Int]
phiN 1 = [1]
phiN n = [ s | s <- [1..n-1], (coPrime s n)]

-- (phiN 2) -> 1
-- (phiN 3) -> 2
-- (phiN 16) -> 8

-- Задача 3. Да се напише функция findSum, която приема
-- като аргумент квадратна числова матрица matrix
-- с размери NxN и връща сумата от числата 𝑥𝑖,
-- където 𝑥𝑖 е максималното измежду всички числа
-- в i-тия ред и i-тия стълб на matrix, i се изменя от 1 до n.

--findSum :: [[Int]] -> Int
-- (findSum [[1, 2, 3], [4, 5, 6], [7, 8, 9]]) -> 24
-- (findSum [[7, 4, 2, 0], [0, 1, 12, 31], [0, 9, 1, 0], [-3, 7, -5, 12]]) -> 81

-- k devisors
isPrime' :: Int -> Bool
isPrime' 1 = False
isPrime' n = length[ x | x <- [2..n - 1], mod n x == 0] == 0

findPrimeDevisors :: Int -> Int
findPrimeDevisors n = length[ x | x <- [1..n], (mod n x) == 0 && isPrime' x]

kDevisors :: Int -> Int -> Int
kDevisors n k = length[ x | x <- [1..n], findPrimeDevisors x == k]

{-
  Зад. 1. Да се напише функция, която намира сбора на две матрици, представени
  като списък от списъци.
-}

addMatrices :: (Num a, Eq a) => [[a]] -> [[a]] -> [[a]]
addMatrices [] [] = []
addMatrices (m:ms1) (n:ms2) = zipWith (+) m n : addMatrices ms1 ms2

{-
  Зад. 2. Да се напише функция, която нулира всички сълбове на матрица, в които
  се съдържа стойност 0. Матрицата е представена като списък от списъци.
-}

cleanCol :: (Num a, Eq a) => [a] -> [a]
cleanCol xs = if elem 0 xs then map (*0) xs else xs

nullify :: (Num a, Eq a) => [[a]] -> [[a]]
nullify matrix@([]:_) = matrix
nullify matrix = zipWith (\ x xs -> x:xs) (cleanCol (map head matrix)) (nullify (map tail matrix))

{-
  Зад. 3. Напишете функция която намира транспонираната на дадена матрица.
-}

myTranspose :: [[Int]] -> [[Int]]
myTranspose [] = []
myTranspose ([] : xss) = myTranspose xss
myTranspose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : myTranspose (xs : [ t | (_:t) <- xss]) 

{-
  Зад. 4. Напишете функция, която намира произведението на две матрици.
-}

--multMat :: Num a => [[a]] -> [[a]] -> [[a]]
--multMat xss yss = [[sum (zipWith (*) xs ys) | ys <- (transpose yss)] | xs <- xss]

{-
  Зад. 5. Напишете функция, която намира броя на дъгите на граф, представен
  чрез матрица на съседство.
-}

countEdges :: [[Int]] -> Int
countEdges graph = sum (map sum graph)

{-
  Зад. 6. Напишете функция, която намира всички листа на дърво, представено
  чрез матрица на съседство.
-}

leafsOfMatrix :: [[Int]] -> [Int]
leafsOfMatrix xss = [ind | (ind, edgesCount) <- zip [0..] (map sum xss), edgesCount == 0]

 