
import Data.Char
import Distribution.Simple.Program (c2hsProgram)
--1.1-----------------------------------------------
inRange :: Int -> Int -> [Int] -> [Int]
inRange a b [] = []
inRange a b (x:xs)
       | (x >= a && x <= b) = x : inRange a b xs
       | otherwise = inRange a b xs

--1.2-----------------------------------------------
countPositive :: [Int] -> Int
countPositive [] = 0 
countPositive (x:xs) 
            | x > 0 = 1 + countPositive xs
            | otherwise = countPositive xs 

--1.3------------------------------------------------
lowercased :: String -> String
lowercased [] = []
lowercased (x:xs) = toLower x : lowercased xs

capitalised :: String -> String
capitalised [] = []
capitalised (x:xs) = toUpper x : lowercased xs 

--1.4-------------------------------------------------
titleAux :: [String] -> [String]
titleAux [] = []
titleAux (x:xs)
     | (length x) >= 4 = capitalised x : titleAux xs
     | otherwise = lowercased x : titleAux xs 

title :: [String] -> [String]
title [] = []
title (x:xs) = capitalised x : titleAux xs

--2---------------------------------------------------
isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = isort [a | a <- xs, a <= x] ++ [x] ++ isort [a | a <- xs, a >x]


--3---------------------------------------------------
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
     | x <= y   =  x : merge xs (y:ys)
     |otherwise =  y : merge (x:xs) ys

merge2 :: (Ord a) => [a] -> [a] -> [a]
merge2 [] [] = []
merge2 xs [] = xs
merge2 [] xs = xs
merge2 (x:xs) (y:ys) = isort ((x:xs) ++ (y:ys))

--4---------------------------------------------------
rotor :: Int-> [Char] -> [Char]
rotor _ [] = []
rotor 0 str =  str
rotor n (c:str)
     | n<0 || n >= length (c:str) = "Error"
     | otherwise = rotor (n-1) (str ++ [c])
     -- drop n str ++ take n str

makeKey :: Int -> [(Char, Char)]
makeKey n = zip ['A'..'Z'] (rotor n ['A'..'Z']) 

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c [] = c
lookUp c (x:xs) 
          | c == fst x = snd x 
          | otherwise = lookUp c xs 
-- lookUp c ((c1,c2),pairs)
--        | c == c1 = c2
--        | otherwise = lookUp c pairs

encipher :: Int -> Char -> Char
encipher 0 c = c
encipher n c = lookUp c (makeKey n)

normalise :: String -> String
normalise [] = []
normalise (c:str) 
          | c `elem` ['a'..'z'] = toUpper c :normalise str
          | c `elem` ['A'..'Z'] || c `elem` ['0'..'9'] = c : normalise str
          | otherwise = normalise str

encipherStr :: Int -> String -> String
encipherStr n [] = []
encipherStr n str = encipherStrAux n (normalise str)
           where encipherStrAux n [] = []
                 encipherStrAux n (c:str) = encipher n c : encipherStrAux n str

encipherAux' :: Int -> String -> String
encipherAux' n [] = []
encipherAux' n (c:str) = encipher n c : encipherAux' n str

encipherStr' :: Int -> String -> String
encipherStr' n str = normalise (encipherAux' n str)

