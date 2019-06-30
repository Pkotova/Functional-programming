import Data.Char
import Data.Maybe
import Data.List

t :: [(Int, [Int])]
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

data BTree = Nil | Node Int BTree BTree
t1 = (Node 10 (Node 1 Nil Nil) (Node 25 (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)) Nil))
t2 = (Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil)))
t3 = (Node 10 (Node 1 Nil Nil) (Node 25 (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)) Nil))

main :: IO()
main = do
 print ((specialSum (5 -) [1..10]) (> 0))
 print ((specialSum (\x -> x + 1) [(-5)..5]) odd)
 print (findUncles t 5)
 print (findUncles t 7)
 print (findUncles t 10)
 print (findUncles t 2)
 print (leavesAreEqual t1 t2)
 print (leavesAreEqual t1 t3)

specialSum :: (Int -> Int) -> [Int] -> ((Int -> Bool) -> Int)
specialSum f xs = \ p -> sum [x ^ 2 | x <- xs, p (f x)]

findUncles :: [(Int, [Int])] -> Int -> [Int]
findUncles tree node = if null parent then [] else brothers (head parent)
 where
  parent = [v | (v, vs) <- tree, elem node vs]
  brothers v = concat [delete v vs | (_, vs) <- tree, elem v vs]

findUncles' :: [(Int, [Int])] -> Int -> [Int]
findUncles' tree node = processParent node (\ (v, _) -> processParent v (\ (_, vs) -> delete v vs))
 where
  processParent v f = case find (\ (_, vs) -> elem v vs) tree of {Nothing -> []; Just p -> f p}

leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual t1 t2 = sort (leaves t1) == sort (leaves t2)
 where
  leaves Nil              = []
  leaves (Node n Nil Nil) = [n] 
  leaves (Node _ lt rt)   = leaves lt ++ leaves rt