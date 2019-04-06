main :: IO()
main = do
 
 print(f [10,20,30,40] [[1,2],[3,4],[5,6]])
 --print(fff [2,4,0,3])
-- print(ff [1,2,3,7,8,9] [3,4,5,6,7])

f :: [Int] -> [[Int]] -> [[Int]]
f l1 l2 = map (\x -> (x:l1)) (map head l2)

