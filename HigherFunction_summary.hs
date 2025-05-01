map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x: map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

foldl' :: (b -> a -> b) -> b-> [a] -> b   -- a is the current element, b is the accumulator
foldl' _ acc [] = acc
foldl' f acc (x:xs) =  foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b -- a is the current element, b is the accumulator
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs) 

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "empty list"
foldr1' _ [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "empty list"
foldl1' _ [x] = x
foldl1' f (x:xs) = foldl f x xs


----------------------------------------------------------------------
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a, b) : abs) = (a:as, b:bs)
    where (as, bs) = unzip' abs

unzipr :: [(a,b)] -> ([a], [b])
unzipr = foldr (\(a, b) (as, bs) -> (a:as, b:bs)) ([], [])

unzipl :: [(a,b)] -> ([a], [b])
unzipl = foldl (\(as, bs) (a, b) -> (a:as, b:bs)) ([], [])

----------------------------------------------------------------------
-- QuickCheck:
-- QuickCheck is a Haskell library for random testing of program properties.
-- start with "prop_" and then use "quickCheck" command argument to test the property
