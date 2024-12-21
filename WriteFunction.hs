
myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n
-- myAbs (-10) is correct command

mySign :: Int -> Int
mySign n | n>0 = 1
         | n == 0 = 0
         | otherwise = -1 

-- Pattern Matching:


sum' :: (RealFloat a) => [a] -> a 
sum' [] = 0 
sum' (x:xs) = x + sum' xs

