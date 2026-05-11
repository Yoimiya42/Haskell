# Concepts

- **QuickCheck**: A tool that tests properties by generating random inputs.

- **Weak properties**: Check only part of the behavior. Easy to pass, but likely to miss bugs.
- **Strong properties**: Check the essential behavior more fully. Harder to pass, but better at catching bugs.

```haskell
prop_reverseTwice :: [Int] -> Bool
prop_reverseTwice xs = reverse (reverse xs) == xs

-- condition ==> property
prop_division :: Int -> Int -> Property
prop_division x y =
  y /= 0 ==> (x `div` y) * y + (x `rem` y) == x
-- condition ==> property
```

- **Curried function**: Takes one argument and returns a function for the next argument.

```haskell
add x y = x + y
add' = \x -> \y -> x + y
```

- **Lambda expression**: An anonymous function written with `\`. Often used for short functions passed to higher-order functions.

```haskell
foldr (\x acc -> x + acc) 0 xs
```

- **Higher-order function**: Takes a function as an argument or returns a function. Examples: `map`, `filter`, `foldr`, `foldl`, `zipWith`.

- **Tail recursion**: The recursive call is the last operation. This can reuse the stack frame and save memory.

```haskell
factorial :: Int -> Int -> Int
factorial n acc
  | n == 0    = acc
  | otherwise = factorial (n - 1) (n * acc)
```

- **Lazy evaluation**: Evaluates an expression only when its result is needed.

```haskell
take 5 [1..]  -- [1,2,3,4,5]
```

- **Pattern matching**: Defines function behavior by matching the structure of the input.

```haskell
describeList :: [a] -> String
describeList []  = "Empty list"
describeList [_] = "One element"
describeList _   = "More than one element"
```

- **Monadic programming**: Sequences computations with context, such as failure (`Maybe`) or I/O (`IO`). `>>=` passes the result of one step to the next.

```haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

result = Just 10 >>= \x -> safeDiv x 2   -- Just 5
```

- **Typeclass**: A set of behaviors that many types can implement.

- **Basic type**: A built-in type, such as `Int`, `Char`, or `Bool`.

- **Polymorphic type**: A type that can work with many different types.

- **Overloading**: Lets one function or operator work on multiple types through a typeclass constraint.

- **Guards**: A way to define functions with multiple conditions using `|`.

- **class**: The keyword used to define a typeclass.

- **Prelude**: Haskell's default standard library. It is imported automatically and includes basic types, common functions, and standard typeclasses.

- **do**: Syntax sugar for monadic code. It lets monadic actions be written in an imperative style.

# Higher-Order Functions

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [] = error "empty list"
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ [] = error "empty list"
foldl1 f (x:xs) = foldl f x xs

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((a, b):pairs) = (a:as, b:bs)
  where
    (as, bs) = unzip pairs

unzip' :: [(a, b)] -> ([a], [b])
unzip' pairs = foldr (\(a, b) (as, bs) -> (a:as, b:bs)) ([], []) pairs

unzip'' :: [(a, b)] -> ([a], [b])
unzip'' pairs =
  let (as, bs) = foldl (\(as, bs) (a, b) -> (a:as, b:bs)) ([], []) pairs
  in (reverse as, reverse bs)
```

# Function Composition

- `$`: Function application. It reduces parentheses. `f $ g x = f (g x)`
- `.`: Function composition. `(f . g) x = f (g x)`. It is common in point-free style.

```haskell
mySum :: Num a => [a] -> a
mySum [] = 0
mySum xs = foldr (+) 0 (map (\x -> x * x) xs)
-- mySum xs = foldr (+) 0 $ map (\x -> x * x) xs
-- mySum = foldr (+) 0 . map (\x -> x * x)
```

# I/O

```haskell
getLine :: IO String
getChar :: IO Char
putStrLn :: String -> IO ()
readLn :: Read a => IO a

-- () means "no meaningful value", like void in C
-- do notation is used to sequence IO actions
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

readAndCheck :: [Int] -> IO Bool
readAndCheck nums = do
  putStrLn "Enter an integer"
  n <- readLn
  if n == 0
    then return (all isEven nums)
    else readAndCheck (n : nums)

testInts :: IO ()
testInts = do
  putStrLn "This program checks if all integers are even"
  putStrLn "Enter an integer per line. Stop by typing 0"
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
safeSqrt x
  | x < 0     = Nothing
  | otherwise = Just (sqrt x)

example = Just 4 >>= safeSqrt >>= safeRecip
-- safeRecip (sqrt 4) = Just 0.5

example2 = Just (-4) >>= safeSqrt >>= safeRecip
-- safeSqrt (-4) = Nothing, so the rest does not run
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
