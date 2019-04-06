main :: IO()
main = do
 
 print(ff [1,2,3,7,8,9] [3,4,5,6,7])


ff :: [Int] -> [Int] -> [Int]
ff [] l2 = l2
ff l1 [] = l1
ff (x:xs) (y:ys)
 | x<y = (x:(ff xs (y:ys)))
 | otherwise = (y:(ff (x:xs) ys))