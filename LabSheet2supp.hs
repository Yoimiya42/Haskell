

--1---------------------------------------------------
-- elemNum :: Eq a => a-> [a]-> Integer
--  so that elemNum x xs returns the number of times that x occurs in the
--  list xs. For example, elemNum 3 [1,2,3,4,3,5,3] returns 3.
elemNum :: Eq a => a -> [a] -> Integer
elemNum _ [] = 0
elemNum n (x:xs)
        | n == x    = 1 + elemNum n xs
        | otherwise = elemNum n xs
--2---------------------------------------------------
-- unique :: [a]-> [a]
--  so that unique xs returns a list of elements of xs that occur exactly once
--  in xs.
--  example: unique [1,2,3,4,3,5,3] returns [1,2,4,5]
unique' :: Eq a => [a] -> [a]
unique' list = uniqueAux' list list

uniqueAux' :: Eq a => [a] -> [a] -> [a]
uniqueAux' [] l2 = []
uniqueAux' (x:xs) l2 
        | elemNum x l2 == 1 = x : uniqueAux' xs l2
        | otherwise = uniqueAux' xs l2 

unique :: Eq a => [a] -> [a]
unique  [] = []
unique all@(x:xs)
        | elemNum x all == 1 = x: unique xs
        | otherwise = unique xs 
       
--3---------------------------------------------------
--  insert :: Ord a => a-> [a]-> [a]
--  which correctly inserts an element into a sorted list in ascending order
--  example: insert 3 [1,2,4,5] returns [1,2,3,4,5]
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) 
        | n <= x = n:x:xs
        | otherwise = x : insert n xs  

--4---------------------------------------------------
-- isort :: Ord a => [a]-> [a]
-- that takes a list and returns a sorted list (and which calls insert)
-- for example, isort [3,1,4,1,5,9,2,6,5,3,5] returns [1,1,2,3,3,4,5,5,5,6,9]


isort :: Ord a => [a] -> [a]
isort [] = [] 
isort (x:xs) = insert x (isort xs)

insert2 :: Ord a => a -> [a] -> [a]
insert2 n [] = [n]
insert2 n (x:xs)
        | n > x = n:x:xs
        | n == x = n:xs
        | otherwise = x: insert2 n xs

isort2 :: Ord a => [a] -> [a]
isort2 [] = []
isort2 (x:xs) = insert2 x (isort2 xs)