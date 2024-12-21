--1111111 map -------------------------------------------------------------
--map:  takes [a function] and [a list] as parameters and applies the function to each element of the list
-- take:      function that take: 'a'
--                      return: 'b'
--            list of  'a'
-- return :   list of  'b'

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 


--2222222 filter-------------------------------------------- 2 f  i  l  t  e  r -------------------------------------------------------------

-- filter: takes a [predicate] and [a list] as parameters and returns the list of elements that satisfy the predicate
-- take:      function that take: 'a'
--                      return: Bool
--            list of  'a'
-- return :   list of  'a'
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
     |f x       = x : filter' f xs  --  if the current element satisfies the predicate, added it to the new list
     |otherwise = filter' f xs


-- Sort a list using list comprehension
quicksort' ::(Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs)  = quicksort' [n | n <- xs, n < x] ++ [x] ++ quicksort' [ n | n <- xs, n >=x ]

-- Now sort a list using filter
quicksort :: (Ord a) =>[a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<x) xs) ++ [x] ++  quicksort (filter (>=x) xs) 


---------------Laziness------------------------------------------------------------------
largestDivisible :: (Integral a) => a 
largestDivisible = head (filter p [100000,99999..])
                where p x = x `mod` 3829 == 0

-- Laziness: head() not handle the list until the first list element is appeared


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | odd n  = n : chain (n * 3 +1)
    | even n = n : chain (n `div` 2)

checkChain :: Int
checkChain = length (filter p (map chain [100,99..]))
        where p list = length list > 15



--3333333 fold ------------------------------------------------- 3 f  o  l  d   -------------------------------------------------
-- fold: 
-- take:        function  takes: two parameters (accumulator, current element) return: a single value
--              a starting value  (as accumulator)
--              a list            (to be folder)
-- return:      a single value 

foldl' :: (b -> a -> b) -> b -> [a] ->b    -- a is the current element, b is the accumulator
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b   -- a is the current element, b is the accumulator
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- e.g.  foldr' (+) 0.5 [1,2,3,4,5] 
{-
=> (+) 1 foldr' (+) 0.5 [2,3,4,5]  
=> (+) 1 (+) 2  foldr' (+) 0.5 [3,4,5]
=> (+) 1 (+) 2 (+) 3 foldr' (+) 0.5 [4,5]
=> (+) 1 (+) 2 (+) 3 (+) 4 foldr' (+) 0.5 [5]
=> (+) 1 (+) 2 (+) 3 (+) 4 (+) 5 foldr' (+) 0.5 []    (foldr' (+) 0.5 [] <=> foldr' _ acc [] = acc  =>0.5)
=> (+) 1 (+) 2 (+) 3 (+) 4 (+) 5 0.5 
-}

--------------------------------  Application for fold --------------------------------------------
elem' :: (Eq a) => a -> [a] -> Bool
elem' n list = foldl (\ acc x -> if x == n then True else acc) False list
-- 'False' is starting value of accumulator

map'' :: (a -> b) ->[a] -> [b]
map'' f list = foldr (\x acc -> f x : acc) [] list

maximum' :: (Ord a) => [a] -> a
maximum' (x:xs) = foldl ( \y acc -> if y > acc then y else acc) x xs

-- [1,2,3,4,5] -> [5,4,3,2,1]
reverse' :: [a] -> [a]
reverse'  = foldl (\ x y -> y : x) [] 

product' :: (Num a) => [a] -> a 
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) [] 

head'' :: [a] -> a
head'' = foldl1 (\x _ -> x)

last'' :: [a] -> a 
last'' = foldr1 (\ _ x -> x)

--444444444  scan ------------------------------------------------- 4 s  c  a  n -------------------------------------------------
-- scan:  similar to fold, but it returns a list of all the intermediate accumulator states
-- take:        function  takes: two parameters (accumulator, current element) return: a single value
--              a starting value  (as accumulator)
--              a list            (to be folder)
-- return:      a list of single value

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' _ acc [] = [acc]
scanl' f acc (x:xs) = acc : scanl' f (f acc x) xs

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ acc [] = [acc]
scanr' f acc (x:xs) = f x (head (scanr' f acc xs)) : scanr' f acc xs



--555555555  $ ------------------------------------------------- 5 $ -------------------------------------------------
-- $:  function application operator
-- take:        a function
--              a parameter
-- return:      a value

($) :: (a -> b) -> a ->b
f $ x = f x 

-- when $  is encountered, the expression on its right is treated as a parameter to the function on its left
-- square (3+4+9) => square $ 3+4+9  $ has the lowest precedence
-- filter (>10) map (*2) [2..10] => filter (>10) $ map (*2) [2..10]


--666666666  . ------------------------------------------------- 6 . -------------------------------------------------
-- .:  function composition operator
-- take:        a function
--              a function
--              a parameter
-- return:      a value

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
-- the type of parameter of f must be the same as the return type of g 

