main :: IO()
main = do

 print ((pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 1)    -- = 8
 print (switchsum (\x -> x + 1) (\x -> x * 2) 1 $ 2 )             -- = 3
 print (switchsum (\x -> x + 1) (\x -> x * 2) 2 $ 2 )             -- = 9
 print (switchsum (\x -> x + 1) (\x -> x * 2) 3 $ 2 )             -- = 16
 print (switchsum (\x -> x + 1) (\x -> x * 2) 4 $ 2 )             -- = 30
 print (replaceAssoc [5,4,2,3] [(1,5),(3,7),(5,9),(7,11),(9,13)]) -- -> [9,4,2,7]
 

-- TASK 01

pairCompose :: [Int -> Int] -> (Int -> Int)
pairCompose [] x = 0                                        --Bottom 
pairCompose (f:g:fs) x = helper + pairCompose fs x          --Using helper for composition
 where helper = (f . g) x
pairCompose (f:fs) x = (f . (\x -> x)) x
--------------------------------------------------------------------------
-- TASK 02

isEven :: Int -> Bool                                      -- Functiong for cheching if the number is even or odd ( or using function even)
isEven 0 = False;
isEven n = (mod n 2 == 0)

function :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
function f g 0 x = 0                                       -- Bottom 0 addends means the sum = 0
function f g 1 x = f x
function f g n x = if isEven n then (g ((function f g (n-1)) x)) 
                   else (f ((function f g (n-1)) x))

switchsum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchsum f g 0 x = 0
switchsum f g n x = (function f g n x) + (switchsum f g (n-1) x) 

--------------------------------------------------------------------------
-- TASK 03

headOf:: [Int] -> Int                                      -- Functiong for returning the head of the list (or using function head)
headOf [] = 0
headOf (x:xs) = x

replaceAssoc :: [Int] -> [(Int,Int)] -> [Int]
replaceAssoc []_ = []                                     -- Bottom
replaceAssoc list [] = list                               
replaceAssoc (x:list) dict = helper : (replaceAssoc list dict)          -- using helper to check the keys
 where helper =  if (length [q | (p, q) <- dict, p == x] == 0) then x   -- checking if there is any element with key(p) == x,- True - return x, 
                 else (headOf [q | (p, q) <- dict, p == x])                                                              --  - False - return the element
                          
--------------------------------------------------------------------------

 
 
 