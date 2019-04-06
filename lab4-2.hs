main :: IO()
main = do

 print (isPrime 2)
 print (f [2,4,0,3])
 print (ff [1,2,3,7,8,9] [3,4,5,6,7])
 
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = length[x | x<-[2..n-1], mod n x == 0] == 0

pythagoresTripples :: Integer->Integer->Integer->[(Integer,Integer,Integer)]
pythagoresTripples a b c = [(a,b,c) | c <- [c..],b <- [b..c],a <- [a..b], a * a + b * b == c*c]



f :: [Int] -> [Int]
f [] = []
f [_] = []
f (x:xs) = (x*(length xs)):(f xs)

ff :: [Int] -> [Int] -> [Int]
ff [] l2 = l2
ff l1 [] = l1
ff (x:xs) (y:ys)
 | x<y = (x:(ff xs (y:ys)))
 | otherwise = (y:(ff (x:xs) ys))