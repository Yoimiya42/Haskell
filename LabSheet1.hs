import Data.Char(toUpper, toLower)
import Data.List



-- Square of a number
square :: Int -> Int
square x = x * x
-- Pyth: Get the sum of squares of two integers
pyth :: Int -> Int -> Int
pyth x y = square x + square y
-- isTriangle: Check if the three sides can form a triangle
isTriangle :: Int -> Int -> Int -> Bool
isTriangle x y z =  pyth x y == square z

isTriangleAny :: Int -> Int -> Int -> Bool
isTriangleAny x y z = isTriangle x y z || isTriangle y z x || isTriangle z x y

halfEvens :: [Int] -> [Int]
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [ x | x <- xs, x >= a, x <= b ]

countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]
----------------------------------------------------------------------
capitalised :: String -> String
capitalised [] = []
capitalised str = toUpper(head str) : tail [toLower x | x <- str]

capitalised2 :: String -> String
capitalised2 [] = []
capitalised2 str = [toUpper(head str)] ++ tail [toLower x | x <- str]

-- capitalised
capitalised3 :: String -> String
capitalised3 [] = [] -- is a pattern that matches an empty list.
capitalised3 (x:xs) = toUpper x : [toLower x | x <- xs] 
-- (x:xs) is a pattern that matches a non-empty list.
-- x is the first element of the first list, and xs is the rest of the list.
---------------------------------------------------------------------
lowercase :: String -> String
lowercase [] = []
lowercase str = [toLower x | x <- str]

title :: [String] -> [String]
title (w:words) = capitalised w : [if length s >=4 then capitalised s else lowercase s | s <- words]

