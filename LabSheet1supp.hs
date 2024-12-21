
fun1 :: (Ord a, Num a) =>[a] -> a
fun1 [] = 0
fun1 (x:xs) = x + 1

fun2 :: (Num a) => [a] -> a
fun2 [] = 0
fun2 (x:[]) = x
fun2 (x1:x2:xs) = x1 + x2

fun1' :: (Ord a, Num a) => [a] ->a
fun1' list 
        | null list = 0
        | otherwise = head list + 1

fun2' :: (Num a) => [a] -> a
fun2' xs = if null xs then 0 
        else if null (tail xs) then head xs
        else head xs + head (tail xs)

firstDigit' :: [Char] -> [Char]
firstDigit' [] = []
firstDigit' (x:xs)
        | x `elem` ['0'..'9'] = [x]
        | otherwise = firstDigit' xs

firstDigit2 :: String -> String
firstDigit2 str 
        | null list = ""
        | otherwise = [head list]
        where list = [ x | x <- str, x `elem` ['0'..'9']]

exOr :: Bool -> Bool -> Bool
exOr False False = False
exOr False True  = True
exOr True False  = True
exOr True True   = False

exOr2 :: Bool -> Bool -> Bool
exOr2 x y = not (x == y)