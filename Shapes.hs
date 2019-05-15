main :: IO()
main = do
 print (area r1)
 print (area c1)
 print (perimeter r1)
 print (perimeter c1)
 print (r1)
 
data Shape = Rectangle Double Double 
             |Circle Double --deriving Show
             |Triangle Double Double Double 
             |Cylinder Double Double 
             
instance Show Shape where
 show (Rectangle a b) = show a ++ " " ++ show b
 show (Circle r) = show r
 
r1 :: Shape
r1 = Rectangle 2 4

c1 :: Shape
c1 = Circle 1

area :: Shape -> Double 
area (Rectangle a b ) = a * b
area (Circle r)       = pi * r * r
area (Triangle a b c) = sqrt (p * (p - a) * (p - b) * (p - c))
 where p = perimeter(Triangle a b c) / 2
area (Cylinder r h) = 2 * area(Circle r) + h + perimeter(Circle r)


perimeter:: Shape -> Double 
perimeter(Rectangle a b) = 2 * (a + b)
perimeter(Circle r) = 2 * pi * r
perimeter(Triangle a b c) = a + b + c
perimeter _               = error "Ne moje" 

isRound :: Shape -> Bool
isRound (Circle _ )     = True
isRound (Cylinder _ _ ) = True 
isRound _               = False

is2D :: Shape -> Bool
is2D (Cylinder _ _ ) = False
is2D _               = True

sumArea :: [Shape] -> Double
