import Data.Char

main :: IO()
main = do
 print "hello"
 --print (normalizeMsg "Attack London tomorrow at a.m.")
 --print (normalizeMsg "Attack London tomorrow at 10 a.m.")
 --print (encode ['A'..'Z'] 'Z' (1) )
-- print (encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM")
 --print (decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR")
 print (crackAll ['A'..'Z'] "FYYFHPQTSITSYTRTWWTBFYYJSFR")
-- task 01
normalizeMsg :: String -> String
normalizeMsg  [] = []
normalizeMsg (x:xs)
 |isDigit x = error "Digits not allowed!"
 |isLower x = toUpper x : normalizeMsg xs
 |isUpper x = x : normalizeMsg xs
 |otherwise = normalizeMsg xs
 --a
encode :: String -> Char -> Int -> Char
encode alphabet ch offset = alphabet !! mod (offset + (indexOf ch alphabet))(length alphabet)
  where
  indexOf :: Char -> String -> Int
  indexOf charche alpha = (helper 0 charche alpha)
   where
    helper _ c ""     = error (c : " unsupported symbol!" )
    helper i c (x:xs) = if c == x then i else helper (i + 1) c xs
 
 --b
encrypt :: [Char] -> Int -> String -> String
encrypt alphabet offset normalized = [encode alphabet c offset | c <- normalized]
-- c
decrypt :: [Char] -> Int -> String -> String
decrypt alphabet offset encrypted = [encode alphabet c (- offset) | c <- encrypted]

-- task 02
--a 
crackAll :: [Char] -> String -> String
crackAll aphabet encrypted = [encode aphabet c | c <- encrypted]