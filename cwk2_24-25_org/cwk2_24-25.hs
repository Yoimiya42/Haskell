

-- 1. Data Types -----------------------------------------------------------

type Horse = [String]

-- 2. Basics ---------------------------------------------------------------

horse :: Horse
horse = [
      "    ,//)     "    -- 13 characters / line
    , "    ;; '\\    "
    , " ,;;' ( '\\   "
    , "    / '\\ _)  "
    ]

myHorse :: Horse
myHorse = [
     "      oooo      "  -- 16 characters / line
    ,"   oo oo o      "
    ,"  oo o    o     "
    ,"oo     ·, oo    "
    ,"o      ''  oo   "
    ,"     o       o  "
    ,"   oo  o o    o "
    ,"  oo      oo  o "
    ," o         ooo  " 
    ]  -- 9 lines 


-- Rotate it 90 degrees to the right
transpose :: Horse -> Horse
transpose [] = []
transpose ([]:_) = []
transpose hor =  (reverse . map head) hor : transpose (map tail hor)

-- Mirror image of a horse
mirror :: Horse -> Horse
mirror = map reverse 

-- Rotate it 180 degrees
transpose180 :: Horse -> Horse
transpose180 = transpose . transpose

-- Rotate it 270 degrees to the right
transpose270 :: Horse -> Horse
transpose270 =  transpose . transpose . transpose



-- 3. Integer Sequences  --------------------------------------------------

-- Tribonacci [0, 1, 1, 2, 4, 7, 13, 24, 44, 81, ...]
tribonacci :: Int -> [Int]
tribonacci n = take n $ tribonacci' 0 1 1
        where tribonacci' a b c = a : tribonacci' b c (a + b + c)

-- lazyCatererSeq [1, 2, 4, 7, 11, 16, 22, 29, 37, 46, ...]
lazyCatererSeq :: Int -> [Int]
lazyCatererSeq n = take n $ lazyCatererSeq' 1 1
        where lazyCatererSeq' k prev = prev : lazyCatererSeq' (k + 1) (prev + k)


-- 4. IO
pretty :: Horse -> IO()
pretty = mapM_ putStrLn

combineHorse :: [Int] -> Horse -> [Horse] 
combineHorse [] _ = []
combineHorse (x:xs) h 
    | x == 0 = combineHorse xs h
    | otherwise = foldl1 (zipWith (++)) (replicate x h) : combineHorse xs h

horseSeq :: (Int -> [Int]) -> Int -> Horse -> IO()
horseSeq f n h = mapM_ pretty $ combineHorse (f n) h



    
-- 5. Monads

--  'Maybe' type already provided in standard libraries.

-- Safe head function
shead :: [a] -> Maybe a
shead []     = Nothing    -- Return Nothing for an empty list
shead (x:_)  = Just x     -- Return Just the first element for a non-empty list

-- Safe tail function
stail :: [a] -> Maybe [a]
stail []     = Nothing    -- Return Nothing for an empty list
stail (_:xs) = Just xs    -- Return Just the rest of the list for a non-empty list


main :: IO()
main = do
    horseSeq tribonacci 5 myHorse