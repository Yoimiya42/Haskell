
-- [2,5,1,4,7,3,9]
maxinum' :: [Int] -> Int
maxinum' [] = error "Empty List"
maxinum' [x] = x
maxinum' (x:xs) = max x (maxinum' xs)
-- x = 2 xs = [5,1,4,7,3,9] 
--          x = 5 xs = [1,4,7,3,9]
--                    x = 1 xs = [4,7,3,9]
--                              x = 4 xs = [7,3,9]
--                                        x = 7 xs = [3,9]
--                                                  x = 3 xs = [9]
--                                                            x = 9 
--                                              max 3 9 => 9
--                                      max 7 9 => 9
--                              max 4 9 => 9
--                      max 1 9 => 9
--              max 5 9 => 9
--      max 2 9 => 9
-- => 9
-- [2,8,6,7,3,4,1,10,0]
maxinum2 :: [Int] -> Int
maxinum2 [] = error "Empty List"
maxinum2 [x] = x
maxinum2 (x:xs)
        | x > maxTail = x
        | otherwise = maxTail
        where maxTail = maxinum2 xs

-- [2,5,1,4,7,3,9]
mininum' :: (Num i, Ord i) => [i] -> i
mininum' [] = error "Empty List"
mininum' [x] = x
mininum' (x:xs)
        | x < mininum' xs  = x
        |otherwise = mininum' xs


mininum2' :: (Num i, Ord i) => [i] -> i
mininum2' [] = error "Empty List"
mininum2' [x] = x
mininum2' (x:xs) = min x (mininum2' xs)

duplicate :: (Num i, Ord i) => i -> a  -> [a] -- duplicate :: Int -> Int -> [Int]
-- Num i  let i can be added or subtracted
-- Ord i  let i can be compared

-- Why a not be constrained? a can be any type.  e.g. Int, Char, String, Bool etc.
duplicate times target
        | times <= 0   = []   -- Ord i is essential for this condition
        | otherwise = target : duplicate (times - 1) target   -- Num i is essential for this condition

-- take' 3 [5,4,3,2,1] => [5,4,3]
take1' :: (Num i, Ord i) => i -> [a] -> [a]--take' :: Int -> [a] -> [a]
take1' _ [] = []
take1' count (x:xs) 
     | count <= 0  = []  -- Ord i  let i can be compared
     | otherwise  = x : take1' (count - 1) xs -- Num i  let i can be added or subtracted 

take2' :: Int -> [a] -> [a]
take2' _ [] = []
take2' count _ 
      | count <= 0  = []
take2' count (x:xs) = x : take2' (count - 1) xs 

-- [5,4,3,2,1] => [1,2,3,4,5]
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- zip [1,2,3] [10,20] => [(1,10), (2,20)]
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (ax:axs) (bx:bxs) = (ax, bx) : zip' axs bxs

-- 4 [1,2,3,4,5]
elem' ::(Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
     | a == x = True  -- Eq a let a can be compared for equality
     | otherwise  = elem' a xs

-- [3,2,1,4,5] => [1,2,3,4,5]
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x]


length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs




-- remove :: (Eq a) => a -> [a] -> [a]
-- remove _ [] = []
-- remove n (x:xs)