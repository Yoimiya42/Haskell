

data Tree a = Leaf a | Node (Tree a) (Tree a)  deriving (Show, Eq)
----------------  Q1.1 --------------------------------------------
numleaves :: Tree a -> Int
numleaves (Leaf _) = 1
numleaves (Node x y) = numleaves x + numleaves y

balanced :: Tree a -> Bool
balanced (Leaf a) = True
balanced (Node x y) = (abs (numleaves x - numleaves y) <= 1) && balanced x && balanced y

----------------  Q1.2 --------------------------------------------
ascSort :: (Ord a) => [a] -> [a]
ascSort [] = []
ascSort (x:xs) = ascSort (filter (<= x) xs) ++ [x] ++ ascSort (filter (> x) xs)

splitList :: [a] -> ([a], [a])
splitList list = (take halfList list , drop halfList list)
                where halfList = length list `div` 2 

buildTree :: Ord a => [a] -> Tree a
buildTree (x:[]) = (Leaf x)
buildTree (x:(y:[])) = (Node (Leaf x) (Leaf y))
buildTree list = (Node (buildTree xs) (buildTree ys))
            where (xs, ys) = splitList $ ascSort list

----------------  Q2 --------------------------------------------

data Expr = Val Int | Add Expr Expr  deriving (Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)


eval :: Expr -> Int
eval = folde id (+)  

size :: Expr -> Int
size = folde (const 1) (+) 