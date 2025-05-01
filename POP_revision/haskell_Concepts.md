# Concepts
**QuickCheck**: A tool to automatically test properties by generating random inputs.

**Weak Properties**: check partial aspects of behaviors, easliy to pass but likely to miss bugs.
**Strong Properties**: check essential and all aspects behaviors, making them harder to pass but more effective at catching bugs.
```haskell
prop_ReverseTwice :: [Int] -> Bool
prop_ReverseTwice xs = reverse (reverse xs) == xs

-- Condition => property
prop_division :: Int -> Int -> Property
prop_division x y =
  y /= 0 => (x `div` y) * y + (x `rem` y) == x
-- Condition => property
```

**Curried function**: Takes one argument and return another function for the next argument.
A function that can take multiple parameters by applying the function
to the first parameter and creates partially applied functions, each of which have a single
parameter.
```haskell
add x y = x + y
add' = \x -> (\y -> x + y)
```

**Lambda term**: Anonymous function, written using `\`, usually used for short-lived functions passed as argument to higher-order functions.
```haskell
foldr (\x acc -> x + acc) 0 xs
```

**HigherOrder function**: Take functions as arguments or return a function.e.g. `map`, `filter`, `foldr`, `foldl`, `zipWith`.

**Tail recursion**: the recursive call is the last operation in the function, allowing reuse of the stack frame, memory efficient.
```haskell
factorial :: Int -> Int -> Int
factorial n acc
  | 0 acc = acc
  | otherwise = factorial (n - 1) (n * acc)
```

**Lazy evaluation**: Only evaluates expressions when their results are needed, which saves computation.
```haskell
take 5 [1..]  -- [1,2,3,4,5]  take values as they are generated
```

**Pattern matching**: define functions behavior based on the structure of input parameters
```haskell
describeList :: [a] -> String
describeList []     = "Empty list"
describeList (x:[]) = "One element"
describeList (x:xs) = "More than one element"
```

**Monadic Programming**: let you sequence operations with context, like computations that might fail (Maybe) or involve IO. Monads use >>= (bind) to pass results from one step to the next.
```haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

result = Just 10 >>= (\x -> safeDiv x 2)   -- Result: Just 5
```

**Typeclass**: A set of behaviors, which can be implemented for one of a number of types

**Basic Type**: A type that is defined inside the Prelude package

**Polymorphic type**: A type that can represent multiple different types

**Overloaded types** allow operations to work on multiple types that share common behaviors. = "polymorphic" types + typeclass constraint.

**Guarded**: A way to define functions with multiple conditions, using `|` to separate them.
**class** is a collection of types that support certain operations, called the methods of the class.
**Prelude**:  Haskell's default standard library that is automatically imported into every programs. Includes **basic types(`Int`, `Maybe`)**, common functions(`map`, `filter`, `foldl`), and standard typeclass (`Eq`, `Ord`, `Show`).

**Do**: Syntax sugar over the Monad typeclass, that allows monadic code to be written in an imperative style.(A sequence of actions can be combined as a single composite action using `do`)

# Higher-Order Functions
```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a-> Bool) -> [a] ->[a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = []
foldr f acc (x:xs) = f x (foldr f acc xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [] = error "empty list"
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

foldl1 :: (a -> a -> a) -> [a] ->a
foldl1 _ [] = error "empty list"
foldl1 f (x:xs) = foldl f x xs

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((a,b):abs) = (a:as, b:bs)
      where (as,bs) = unzip abs

unzip' :: [(a,b)] -> ([a], [b])
unzip' abs = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([], []) abs

unzip'' :: [(a, b)] -> ([a], [b])
unzip'' abs = foldl (\(as, bs) (a, b) -> (a:as, b:bs)) ([], []) abs
``` 

# Function Composition
`$`: **function application** just a way to reduce the parentheses `f $ g x = f (g x)`
`.`: **function composition**used in point-free(pointless) style, `(f . g) x = f (g x)`, which avoids explicit arguments.
```haskell
mySum :: Num a => [a] -> a
mySum [] = 0
mySum xs = foldr (+) 0 (map (\x -> x * x) xs)
-- mySum xs = foldr (+) 0 $ map (\ x -> x * x) xs
-- mySum = foldr (+) . map (\ x -> x * x)
```

# I/O 
```haskell
getLine :: IO String 
getChar :: IO Char
putStrLn :: String -> IO ()
putStrLn :: String -> IO ()

 -- () means no meaningful value, like void in C
-- do notation is used to sequence IO actions
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0


readAndCheck :: [IO Int] -> IO Bool
readAndCheck nums = do
  putStrLn "Enter an integer"
  n <- getInt
  if n == 0
    then return (all isEven nums)
    else readAndCheck (n:nums)

testInts :: IO ()
testInts = do
  putStrLn "This program checks if all integers are even"
  putStrLn "Enter an integer per line. Stop with typing 0"
  result <- readAndCheck []
  putStrLn $ "Result: " ++ show result
    if result
      then putStrLn "All numbers you entered are even."
      else putStrLn "Not all numbers you entered are even."
```

## Curried Functions
```haskell
add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' x = \y -> x + y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y
```

# Monad
```haskell
safeRecip :: Double -> Maybe Double
safeRecip 0 = Nothing
safeRecip x = Just (1 / x)

safeSqrt :: Double -> Maybe Double
safeSqrt x | x < 0     = Nothing
           | otherwise = Just (sqrt x)

example = Just 4 >>= safeSqrt >>= safeRecip
-- 相当于：safeRecip(sqrt(4)) = Just 0.5

example2 = Just (-4) >>= safeSqrt >>= safeRecip
-- safeSqrt (-4) = Nothing，后面就不会执行
```
---
```haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeDivide :: IO ()
safeDivide = do
  putStrLn "Enter numerator:"
  a <- readLn
  putStrLn "Enter denominator:"
  b <- readLn
  case safeDiv a b of
    Nothing  -> putStrLn "Error: Cannot divide by zero."
    Just val -> putStrLn $ "Result: " ++ show val

```

