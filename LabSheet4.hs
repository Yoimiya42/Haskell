---  Q1 --------------------------------------------
fun1_1 :: [Int] -> [Int]
fun1_1 [] = []
fun1_1 ns = map (\x -> x * x) ns

fun1_2 :: [Int] -> Int
fun1_2 = foldr (+) 0 . fun1_1

fun1_3 :: [Int] -> Bool
fun1_3 = foldr (\x acc-> x>0 && acc) True

---  Q2 --------------------------------------------
fun2_1 :: (Ord a) => (Int -> a) -> Int -> a
fun2_1 f n = foldr1 (\ x acc -> if x > acc then acc else x) $ map f [0..n]

fun2_2 :: (Ord a) => (Int -> a) -> Int -> Bool
fun2_2 f n = foldr (\x acc -> x /= head (map f [0..n])  && acc) True $ map f [0..n]

fun2_3 :: (Ord a, Num a) => (Int -> a) -> Int -> Bool
fun2_3 f n = foldr(\x acc -> x > 0 && acc) True $ map f [0..n]

fun2_4 :: (Ord a) => (Integer -> a) -> Integer -> Bool
fun2_4  f n = foldl (\acc y -> if y > head (map f [0..n]) then False else acc) True $ map f [0..n]

---  Q3 --------------------------------------------
twice :: (a -> a) -> a -> a
twice f x  = f $ f x

--- Q4 --------------------------------------------
iter :: (Num a, Eq a) => a -> (a -> a) -> a -> a
iter 0 _ x = 1
iter n f x = f (iter (n-1) f x)

--- Q5 --------------------------------------------
iterDouble :: Int -> Int
iterDouble 0 = 2
iterDouble n = iter (n-1) (\x -> x *2) 2
--- Q6 --------------------------------------------
data RhType = Pos | Neg deriving (Show, Eq, Read)

data ABOType = AB | A | B | O deriving (Show, Eq, Ord, Enum, Read)

data BloodType = BloodType ABOType RhType deriving (Show, Eq, Read)

patient1 :: BloodType 
patient1 = BloodType A Pos

patient2 :: BloodType 
patient2 = BloodType B Neg

patient3 :: BloodType 
patient3 = BloodType AB Pos

patient4 :: BloodType 
patient4 = BloodType O Neg

patient5 :: BloodType 
patient5 = BloodType O Pos

showRh :: RhType -> String
showRh Pos = "Positive"
showRh Neg = "Negative"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++" "++ showRh rh

readPatientABO :: BloodType -> ABOType
readPatientABO (BloodType abo rh) = read(showABO abo)

canDonateTo :: ABOType -> ABOType -> Bool
canDonateTo donor recipient 
            | donor == B && recipient == A = False 
            | otherwise = donor >= recipient

--- Q8 --------------------------------------------

data Shape = Triangle {
    base :: Float, height :: Float 
} | Rectangle {
    width :: Float, length :: Float
} deriving (Show, Eq)

area :: Shape -> Float
area (Triangle b h) = b * h *0.5
area (Rectangle w l) = w * l