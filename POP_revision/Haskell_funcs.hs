
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort less ++ [x] ++ quicksort greater
      where less = [a | a <- xs, a <= x]
            greater = [b | b <- xs, b > x]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' (x:xs) = quicksort' (filter (<= x) xs) 
                    ++ [x] ++ 
                    quicksort' (filter (> x) xs)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = [] 
mergesort [x] = [x]
mergesort xs =  merge (mergesort left) (mergesort right)
          where 
            (left, right) = splitAt (length xs `div` 2) xs

                merge :: (Ord a) => [a] -> [a] -> [a]
                merge [] ys = ys
                merge xs [] = xs 
                merge (x:xs) (y:ys)
                      | x <= y   = x: merge xs (y:ys)
                      |otherwise = y: merge (x:xs) ys

data BST a = Empty | Node a (BST a) (BST a)

search :: (Ord a) => a -> BST a -> Bool
search _ Empty = False
search x Node value left right 
      | x < value  = search x left
      | x > value  = search x right
      | otherwise  = True


insert :: (Ord a) => a -> BST a -> Bst a
insert x Empty = Node x Empty Empty
insert x Node value left right
      | x < value  = Node value (insert x left) right
      | x > value  = Node value left (insert x right)
      | otherwise  = Node value left right
