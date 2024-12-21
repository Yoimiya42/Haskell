
map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x: map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

foldl' :: (b -> a -> b) -> b-> [a] -> b   -- a is the current element, b is the accumulator
foldl' _ acc [] = acc
foldl' f acc (x:xs) =  foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b -- a is the current element, b is the accumulator
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs) 

