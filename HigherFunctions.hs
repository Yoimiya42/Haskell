import System.Win32 (COORD(yPos, xPos))


{-
a function with name:
square :: Int -> Int
square x = x * x
-}

{-
lambda function : a function without name
\x -> x * x
Syntax Structure:   \ [parameter(s)] -> [function body]
-}

-- Curried Functions: a function can take multiple parameters

-- !!! Only one parameter accepted at a time in official function

-- A function has been curried
add :: Int -> Int -> Int -> Int
add x y z = x + y + z

-- actual composition:
addTri :: Int -> (Int -> (Int -> Int))
addTri = \x -> \y -> \z -> x + y +z

-- when you call this function using command in terminal:
-- >> add 3 4 5

-- The actual process is :

----------------------------- proc1 -----------------------------
-- accept: 3
-- return: a function named "add 3" 
addTri3 :: Int -> (Int -> Int)
addTri3 = \y -> \z -> 3 + y + z
----------------------------- proc2 -----------------------------
-- accept: 4
-- return: a function named "add 3 4"
addTri3_4 :: Int -> Int
addTri3_4 = \z -> 7 + z
----------------------------- proc3 -----------------------------
-- accept: 5
-- return: 12

-- Syntax Structure of "->":  [parameter] ->  [return (a value or function)] 

square :: Int -> Int
square x = x * x

square' :: Int -> Int
square' = (^2)             -- (^)2   <=> 2^x
 
add' :: Int -> Int -> Int 
add' x y = x + y

add'' :: Int -> Int -> Int 
add'' = (+)
-- In terminal:   5 `add'' `6  <=> add'' 5 6   all work

------------------------------------------------------------------------------------

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' ::(a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x 

flip''' :: (a -> b -> c) -> b -> a ->c
flip''' f = \ x y -> f y x
----------------------------------------------------------------------------------------

