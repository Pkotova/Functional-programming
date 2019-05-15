import Data.List
import Data.Char
main :: IO()
main = do
 
 print 1
 print (distance (p1,p1))
 
data Point = PointFlat Double Double |
             PointSpace Double Double Double
instance Show Point where
 show (PointFlat x y) = show x ++ " " ++ show y
 show (PointSpace x y z) = show x ++ " " ++ show y ++ " " ++ show z
 
 
p1 :: Point
p1 = PointFlat 2 4

p2 :: Point
p2 = PointSpace 1 2 3

distance :: (Point,Point) -> Double
distance ((PointFlat x y),(PointFlat x1 y1))        = sqrt((x - x1)^2 + (y - y1)^2)
distance ((PointSpace x y z),(PointSpace x1 y1 z1)) = sqrt((x - x1)^2 + (y - y1)^2 + (z - z1)^2)
distance _ _                                        = error "Invalid points" 


closestPoint :: [Point] -> Point -> Point
closestPoint []_      = error "empty"
--closestPoint lst p  
 
