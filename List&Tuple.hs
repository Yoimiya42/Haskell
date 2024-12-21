-- First Line: Function Signature
--             Function Name :: Type Input -> Type Output
-- Second Line: Function Definition:
--             Function Name = Function Body

-- Compile and run in terminal:
-- type 'ghci' in terminal which opened at the directory where the file is saved
-- Prelude> type ':l FileName.hs' to load the file
-- Prelude> type the function name you desire to run to see the output

----------------------------------------------------------------------
-- [1,2,3] ++ [4]  True
-- [1,2,3] ++ 4  False
-- [1,2,3] : [4]   -> [1,2,3,4]
-- let a = [4,5,6], [0,1], [2,3,7]
-- [9,9,9] : a     -> [[9,9,9], [4,5,6], [0,1], [2,3,7]]

--ghci> let a = [[4,5,6], [0,1], [2,3,7]]

--ghci> a
--[[4,5,6],[0,1],[2,3,7]]


--ghci> a ++ [[8,8,8]]
--[[4,5,6],[0,1],[2,3,7],[8,8,8]]
--ghci> a
--[[4,5,6],[0,1],[2,3,7]] 
--ghci> [6,6,6] : a
--[[6,6,6],[4,5,6],[0,1],[2,3,7]]
--ghci> a !! 1
--[0,1]

----------------------------------------------------------------------
--                          Lists Functions
-- ghci> head [1,2,3,4,5] -> 1
-- ghci> tail [1,2,3,4,5] -> [2,3,4,5]
-- ghci> last [1,2,3,4,5] -> 5
-- ghci> init [1,2,3,4,5] -> [1,2,3,4]
-- ghci> length [1,2,3,4,5] -> 5

-- ghci> null [] -> True
-- ghci> null [l,2,3,4] -> False    'null' checks if the list is empty

-- ghci> reverse [1,2,3,4,5] -> [5,4,3,2,1]

-- ghci> take 3 [1,2,3,4,5] -> [1,2,3]
-- 'take' extracts the specified number of elements from the beginning of the list

-- ghci> drop 3 [1,2,3,4,5] -> [4,5]
-- ghci> drop 100 [1,2,3,4,5] -> []
-- 'drop' drops the specified number of elements from the beginning of the list

-- ghci> maximum [8,9,10,78,99] -> 99
-- ghci> minimum [8,9,10,78,99] -> 8

-- ghci> sum [1,2,3,4,5,6,7,8] -> 36
-- ghci> product [1,2,3,4,5] -> 120

-- ghci> 4 'elem' [1,2,3,4,5] -> True
-- ghci> 10 'elem' [1,2,3,4,5] -> False
-- 'elem' (element) checks if the elements is present in the list 
----------------------------------------------------------------------
--      List Comparisons
-- compared in lexicographical order

-- ghci> [7,9,10] < [7,9,11] True
-- ghci> [7,9,10] < [7,9,10] False
-- ghci> [7,9,10] < [7,9,9] False 
-- ghci> [7,9,10] < [7,9,10,1] True 
-- ghci> [7,9,10] < [7,9,10,0] True  
-- nonempty list is always greater than an empty list
----------------------------------------------------------------------
--      Ranges and  Steps

--  ['h'..'n'] -> "hijklmn"
--  [0,10] -> [0,1,2,3,4,5,6,7,8,9,10]
--  [3,6..20] -> [3,6,9,12,15,18]
--  [20,19,..10] -> [20,19,18,17,16,15,14,13,12,11,10]
----------------------------------------------------------------------
-- Laziness and Infinite lists

-- take 3 [5,10..] -> [5,10,15]
-- take 10 (cycle [7,8,77]) -> [7,8,77,7,8,77,7,8,77,7]
-- take 10 (repeat 5) -> [5,5,5,5,5,5,5,5,5,5] 
-- replicate 3 10 -> [10,10,10]     
----------------------------------------------------------------------
-- List Comprehensions

-- [x*2 | x<- [1..10]] -> [2,4,6,8,10,12,14,16,18,20]
-- [x*2 | x<- [50..100], x 'mod' 5 == 2, even x] -> [52,62,72,82,92]
-- [if x > 10 then "Big" else "Small" | x <- [7..13], odd x]  -> ["Small", "Small", "Big", "Big"]
-- [x*y | x <- [2,5,10] y <- [5,10,20]]  -> [10,20,40,25,50,100,50,100,200]

-- Nested List Comprehensions

-- ghci> let xss = [[1,3,2,7,8], [5,7,8,10], [10,15,40,55]]
-- [[x | x <- xs, even x] | xs <- xss]] -> [[2,8], [8,10], [10,40]]
----------------------------------------------------------------------
-- Summary: lists have homogeneous elements (every element must belong to the same type)
--                have flexible size: grow and shrink
--          tuple have heterogeneous elements (can contain elements of different types)
--                have fixed size
----------------------------------------------------------------------
-- zip function:  takes two lists and zips them together into one list by joining the matching elements into pairs.

-- ghci> zip [1..10][10,20,30,40,50] -> [(1,10), (2,20), (3,30), (4,40), (5,50)] 

-- (The longest list is truncated to match the shorter one)
----------------------------------------------------------------------
triangle :: [(Int, Int, Int)] 
triangle = [(a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2, a + b + c == 24]

