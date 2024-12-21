
-- Q1 --------------------------------------------
mult :: [Int] -> Int
mult =  foldl1 (\x y -> x * y) 

-- mult :: (Num a) => [a] -> a 
-- mult =  foldr (*) 1
-- Q2 --------------------------------------------
posList :: [Int] -> [Int]
posList = filter (>0) 
-- Q3 --------------------------------------------
trueList :: [Bool] -> Bool
trueList = foldl (&&) True
-- Q4 --------------------------------------------
evenList :: [Int] -> Bool
evenList = foldl (\acc x -> acc && even x) True

-- evenList :: [Int] -> Bool
-- evenList = foldr ((&&). even) True
-- Q5 --------------------------------------------
maxList :: (Ord a) => [a] ->a
maxList = foldl1 (\acc x -> if x > acc then x else acc)
-- Q6 --------------------------------------------
inRange :: Int -> Int -> [Int] -> [Int]
inRange n1 n2 list = filter (\x -> x <= n2 && x >=n1) list
-- Q7 --------------------------------------------
countPositives :: [Int] -> Int
countPositives =  length . filter (>0) 

countPositives' :: [Int] -> Int
countPositives' =  foldr (+) 0 . map (\x -> 1) .filter (\x ->x>0)
-- Q8 --------------------------------------------
myLength :: [Int] -> Int
myLength  = foldl (+) 0 . map (const 1)
-- Q9 --------------------------------------------
myMap :: (a -> b) -> [a] -> [b]
myMap  f = foldr (\x acc -> f x:acc) [] 
-- Q10 --------------------------------------------

myLengthFoldr :: [Int] -> Int
myLengthFoldr = foldr (\ _ acc -> acc + 1) 0 

myLengthFoldl :: [Int] -> Int
myLengthFoldl = foldl (\ acc _ -> acc + 1) 0















--------------------------------------------
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
        | p x = x : filter' p xs
        | otherwise = filter' p xs