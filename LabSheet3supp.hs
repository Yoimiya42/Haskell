
-- Q1 --------------------------------------------
fun1_1 :: [Integer] -> [Integer]
fun1_1  = map (\ y -> y * y ) 

fun1_2 :: [Integer] -> Integer
fun1_2  = foldl (+) 0 . fun1_1

fun1_3 :: [Integer] -> Bool
fun1_3  = foldl (\acc y -> if y <= 0 then False else acc) True . fun1_1

-- fun1_3' :: [Integer] -> Bool
-- fun1_3' = foldl (&&) True. map (>0)

-- Q2 --------------------------------------------
fun2_1 :: (Ord a) => (Integer -> a) -> Integer -> a
fun2_1 f n = foldl1 (\y acc -> if y < acc then y else acc) $ map f [0..n]

fun2_2 :: (Eq a) => (Integer -> a)-> Integer -> Bool
fun2_2 f n = foldl (\acc y -> if y /= head (map f [0..n]) then False else acc) True $ map f [0..n]

fun2_3 :: (Ord a, Num a) => (Integer -> a) -> Integer -> Bool
fun2_3 f n =  foldl (&&) True $ map (>0) $ map f [0..n]

fun2_4 :: (Ord a) => (Integer -> a) -> Integer -> Bool
fun2_4  f n = foldl (\acc y -> if y > head (map f [0..n]) then False else acc) True $ map f [0..n]

-- Q3 --------------------------------------------
twice :: (a->a) -> a ->a
twice f x  = (f . f)x

-- Q4 --------------------------------------------
iter :: Integer -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x = f (iter (n-1) f x )

iter' :: Integer -> (a -> a) -> a -> a
iter' 0 f x = x
iter' n f x = iter (n-1) f $ f x

-- Q5 -------------------------------------------

double :: Integer -> Integer
double x = 2 * x

iterDouble :: Integer -> Integer
iterDouble 0 = 1
iterDouble n = iter (n-1) double 2  