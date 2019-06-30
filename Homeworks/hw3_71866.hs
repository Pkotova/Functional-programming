main :: IO()
main = do
 print 1
-----------------------------------
          -- TASK 01
-----------------------------------
                      
isMember :: Int -> [Int] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs
   
numOfNodes :: [(Int, [Int])] -> Int
numOfNodes [] = 0
numOfNodes tree = length [x | (x, y) <- tree, (sum y) == (parent x)]
 where 
 parent x = if (length[u | (u, v) <- tree, isMember x v] == 0) then 0 
            else head [u | (u, v) <- tree, isMember x v]
            
--------------------------------------
         -- ТАСК 02
--------------------------------------
data Measuring = Temp Int Float 

getDay::Measuring -> Int
getDay(Temp day temperature) = day

getTemperature::Measuring -> Float
getTemperature(Temp day temperature) = temperature

getAverageTemperature::[Float] -> Float
getAverageTemperature [] = 0.0
getAverageTemperature list = (sum list) / (fromIntegral (length list))

closestToAverage :: [Measuring] -> Int
closestToAverage [] = 0
--closestToAverage measurings = 
 

---------------------------------------
            -- TASK 03
---------------------------------------
data BTree = Empty | Node Int BTree BTree deriving Show

height :: BTree -> Int
height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)

leftTree :: BTree -> BTree
leftTree (Node n left right) = left

rightTree :: BTree-> BTree 
rightTree (Node n left right) = right

node :: BTree -> Int
node (Node n left right) = n

hasGrandFather :: BTree -> Bool
hasGrandFather tree = (height tree > 2) 

allNodes :: BTree -> [Int]
allNodes Empty = []
allNodes (Node x l r) = [x] ++ (allNodes l) ++ (allNodes r)

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Empty = False
grandchildrenIncreased tree
 | hasGrandFather tree == False = True
 | otherwise = (grandchildrenIncreased (leftTree tree)) 
            && (grandchildrenIncreased (rightTree tree)) 
            && (allOf (allNodes tree )((node tree) - 1))
            
allOf::[Int] -> Int -> Bool
allOf [] _ = False
allOf list n = (length[ x | x <- list, n <= x] /= 0)




