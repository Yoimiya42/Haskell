
-- Type Declaration  (Type Synonyms or Type Aliases)
type Pos = (Int,Int)
-- "type" is used to define a new type name for some existing type
--  just make a synonym for an existing type, like a nickname
origin :: Pos
origin = (0,0)

type Pair a = (a, a)
-- type declaration with type parameter 'a'

mult :: Num a => Pair a -> a 
mult (m, n) = m * n

copy :: a -> Pair a
copy x = (x,x)

-- Data Declaration  (New Data Type)
-- data [type_name] = [value_constructor1] | [value_constructor2] | ...
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving (Show) -- Show is a type class, it is used to convert a value to a string
-- 'deriving (show)' is used to make the data type an instance of the Show type class
-- Haskell first runs the show function to get the string representation of the value 
-- and then it prints that string to the console

-- we create a new data type called Shape
-- Shape has two value constructors: Circle , Rectangle

-- Circle takes three Float as parameters : center_x, center_y, radius
-- Rectangle takes four Float as parameters: upper_left_x, upper_left_y,  lower_right_x, lower_right_y

-- Value constructors are functions that create ultimately return a value of the data type

-- ghci> :t Circle 
-- Circle :: Float -> Float -> Float -> Shape
-- ghci> :t Rectangle
-- Rectangle :: Float -> Float -> Float -> Float -> Shape
surface :: Shape -> Float  -- Circle is not a type, it is a value constructor; Shape is a type
surface (Circle _ _ r) = pi * r ^2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)


data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)
-- ghci> :t Circle'
-- Circle' :: Point -> Float -> Shape'
-- ghci> :t Rectangle'
-- Rectangle' :: Point -> Point -> Shape'

-- ghci> Circle' (Point 10 20) 30
-- Circle' (Point 10.0 20.0) 30.0

--------------------------- Record Syntax --------------------------------
-- Record Syntax is a way of creating data types with named fields

-- data Person = Person String String Int Float String String deriving (Show)
-- =>
data Person = Person { firstName :: String
                        , lastName :: String
                        , age :: Int
                        , height :: Float
                        , phoneNumber :: String
                        , flavor :: String
                    } deriving (Show)
-- write the name of filed and then specify the type.

-- ghci> :t firstName
-- firstName :: Person -> String
-- ghci> :t age
-- age :: Person -> Int

------------------------- Type Parameters -------------------------------

data Maybe a = Nothing | JUst a
-- 'a' is a [type parameter]
-- 'Maybe' is a [Type Constructor]
-- if we pass 'Int' as the type parameter,  we get a type of "Maybe Int", the value Just 'a' has a type of "Maybe Int"
-- if we pass 'String' as the type parameter, we get a type of "Maybe String", the value Just 'a' has a type of "Maybe String"

-- ghci> :t Just "Haha"
-- Just "Haha" :: Maybe [Char]
-- ghci> :t Just 84
-- Just 84 :: Num a => Maybe a


-- ghci> :t Nothing
-- Nothing :: Maybe a   (Its type is polymorphic)

data Car a b c = Car {
    company :: a,
      model :: b,
       year :: c
} deriving (Show)


tellCar :: (Show t) => Car String String t -> String
tellCar (Car c m y) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y 

-- ghci> tellCar (Car = {company = "Ford", model = "Mustang", year = 1967})
-- "This Ford Mustang was made in 1967"
-- ghci> tellCar (Car = {company = "Ford", model = "Mustang", year = "nineteen sixty seven"})
-- "This Ford Mustang was made in \"nineteen sixty seven\""

-- ghci> :t Car
-- Car :: a -> b -> c -> Car a b c
-- ghci> :t Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967 :: Car [Char] [Char] c
-- ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"
-- Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]

------------------------- Deriving Instances -------------------------------

data Laptop =  Laptop {
      brand :: String,
      gpu :: Int,
      price :: Float
} deriving (Eq, Show, Read)

-- keyword 'deriving' makes the data type an instance of the Eq typeclass 
-- or say that we derive the Eq instance for the Laptop type

-- Note:the types of all the fields in the data type must also be part of the Eq typeclass
-- But at here, Int, String and Float are all part of the Eq typeclass
----------------------------------------------------------------
-- ghci> let laptop1 = Laptop {brand = "Dell", gpu = 4070}
-- ghci> let laptop2 = Laptop {brand = "Dell", gpu = 4070}
-- ghci> laptop1 == laptop2
-- True

-- ghci> let laptop3 = Laptop {brand = "Dell", gpu = 4090}
-- ghci> let laptop4 = Laptop {brand = "Lenovo", gpu = 4090}
-- ghci> laptop3 == laptop4
-- False

-- ghci> let laptop5 = Laptop {brand = "ASUS", gpu = 4090, price = 9999.99}
-- ghci> let laptop6 = Laptop {brand = "ASUS", gpu = 4070, price = 8999.99}
-- ghci> laptop5 == laptop6
-- False

-- ghci> let laptop6 = Laptop {brand = "ASUS", gpu = 4070, price = 8999.99}
-- ghci> "My laptop is "++ show laptop6
-- "My laptop is Laptop {brand = \"ASUS\", gpu = 4070, price = 8999.99}"

-- ghci> read "Laptop {brand = \"ASUS\", gpu = 4090, price = 9999.99}" :: Laptop
-- Laptop {brand = "ASUS", gpu = 4090, price = 9999.99}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
          deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- 'Ord' is for ordering, the data type is now an instance of the Ord typeclass
-- the first value constructor is the smallest and the last is the largest

-- 'Enum' is for things that have predecessors and successors
-- 'Bounded' is for things that have a lowest and highest possible value.
----------------------------------------------------------------
-- ghci> :t Wednesday
-- Wednesday :: Day
-- ghci> Thursday                       (Show)
-- Thursday
-- ghci> read "Saturday" :: Day         (Read)
-- Saturday
-- ghci> Saturday == Wednesday          (Eq)
-- False
-- ghci> Thursday == Thursday           (Eq)
-- True
-- ghci> Thursday > Wednesday           (Ord)
-- True
-- ghci> Saturday `compare` Tuesday     (Ord)
-- GT

-- ghci> minBound :: Day                (Bounded)
-- Monday
-- ghci> maxBound :: Day                (Bounded)
-- Sunday
-- ghci> succ Wednesday                 (Enum)
-- Thursday
-- ghci> pred Saturday                  (Enum) 
-- Friday
-- ghci> [Wednesday .. Sunday]          (Enum) 
-- [Wednesday,Thursday,Friday,Saturday,Sunday]
-- ghci> [minBound .. maxBound] :: [Day]  (Enum, Bounded)
-- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]


---------------------------- Recursive Data Structures -------------------------------
data List a = Empty | Cons a (List a)   deriving (Show, Read, Eq, Ord)
--Cons is another word for ':'
--so actually ':' is a constructor that takes a [value] and a [list] and returns a [list]

-- ghci> Empty
-- Empty
-- ghci> Cons 5 Empty
-- Cons 5 Empty                                  -- 5:[]
-- ghci> Cons 4 (Cons 3 (Cons 2 Empty))
-- Cons 4 (Cons 3 (Cons 2 Empty))             -- 4:3:2:[]

infixr 5 :-:   
-- infix is used to define a new operator
-- 5 is the precedence of the operator. (e.g. * has a fixity of 7 and + has a fixity of 6)
-- infixr is for right-associative and infixl is for left-associative

data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)

data Nat = Zero | Succ Nat   deriving (Show, Read, Eq, Ord)
-- Used to represent natural numbers by recursion data structure

-- Nat is a natural number
-- Succ represents the successor function (+1)

-- Succ (Succ Zero) represents 2

natInt :: Nat -> Int
natInt Zero = 0
natInt (Succ n) = 1 + natInt n

-- ghci> natInt (Succ (Succ (Succ (Succ Zero))))
-- 4

intNat :: Int -> Nat
intNat 0 = Zero
intNat n = Succ (intNat(n-1))

-- ghci> intNat 4
-- Succ (Succ (Succ (Succ Zero)))

addNat :: Nat -> Nat -> Nat
addNat m n = intNat (natInt m + natInt n)
-- ghci> add (Succ Zero) (Succ (Succ (Succ Zero)))
-- Succ (Succ (Succ (Succ Zero)))

addInt :: Nat -> Nat -> Int
addInt m n = natInt(intNat (natInt m + natInt n))
-- ghci> addInt (Succ Zero) (Succ (Succ (Succ Zero)))
-- 4


-- a tree is either an empty tree or a node that contains some value and two trees
data Tree a = EmptyTree | Node a (Tree a)(Tree a) deriving (Show, Read, Eq)

-- make a singleton tree
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
    | x == y = Node x left right
    | x < y  = Node y (treeInsert x left) right
    | x > y  = Node y left (treeInsert x right)


buildTree :: (Ord a) => [a] -> Tree a 
buildTree = foldr treeInsert EmptyTree

-- ghci> buildTree [8,6,4,2,7,3,5]
{-
=> treeInsert 8 Foldr treeInsert EmptyTree [6,4,2,7,3,5]
=> treeInsert 8 treeInsert 6 Foldr treeInsert EmptyTree [4,2,7,3,5]
=> treeInsert 8 treeInsert 6 treeInsert 4 Foldr treeInsert EmptyTree [2,7,3,5]
=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 Foldr treeInsert EmptyTree [7,3,5]
=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 treeInsert 7 Foldr treeInsert EmptyTree [3,5]
=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 treeInsert 7 treeInsert 3 Foldr treeInsert EmptyTree [5]
=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 treeInsert 7 treeInsert 3 treeInsert 5 Foldr treeInsert EmptyTree []   --Foldr treeInsert EmptyTree [] = EmptyTree
=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 treeInsert 7 treeInsert 3 treeInsert 5 EmptyTree

=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 treeInsert 7 treeInsert 3 (Node 5 EmptyTree EmptyTree)

=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 treeInsert 7 (Node 5 (treeInsert 3 EmptyTree) EmptyTree)
=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 treeInsert 7 (Node 5 (Node 3 EmptyTree EmptyTree) EmptyTree)

=> treeInsert 8 treeInsert 6 treeInsert 4 treeInsert 2 (Node 5 (Node 3 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree))

=> treeInsert 8 treeInsert 6 treeInsert 4 (Node 5 (treeInsert 2 (Node 3 EmptyTree EmptyTree)) (Node 7 EmptyTree EmptyTree))
=> treeInsert 8 treeInsert 6 treeInsert 4 (Node 5 (Node 3 (treeInsert 2 EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree))
=> treeInsert 8 treeInsert 6 treeInsert 4 (Node 5 (Node 3 (Node 2 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree))

=> treeInsert 8 treeInsert 6 (Node 5 (treeInsert 4 (Node 3 (Node 2 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree)))
=> treeInsert 8 treeInsert 6 (Node 5 (Node 3 (Node 2 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 EmptyTree EmptyTree))

=> treeInsert 8 (Node 5 (Node 3 (Node 2 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (treeInsert 6 (Node 7 EmptyTree EmptyTree)))
=> treeInsert 8 (Node 5 (Node 3 (Node 2 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (treeInsert 6 EmptyTree) EmptyTree))
=> treeInsert 8 (Node 5 (Node 3 (Node 2 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) EmptyTree))

=> (Node 5 (Node 3 (Node 2 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (treeInsert 8 (Node 7 (Node 6 EmptyTree EmptyTree) EmptyTree)))
=> (Node 5 (Node 3 (Node 2 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (treeInsert 8 EmptyTree)))
=> (Node 5 (Node 3 (Node 2 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree)))
-}

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
      | x == y = True
      | x < y  = treeElem x left
      | x > y  = treeElem x right


data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show, Read, Eq)


