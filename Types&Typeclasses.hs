--Int : Integer with maximum 2^29-1 and minimum -2^29
--Integer : Infinite precision integer

-- Float : Single precision floating point number
-- Double : Double precision floating point number

-- Char : represent a character
-----------------------------------------
-- We can use ':t' to check the type of a variable

--ghci> :t fst
--fst :: (a, b) -> a

--ghci> :t (98, 12.0, "Hello", 'a')
--(12,23.5,"Yoi", 's'):: (Fractional b, Num a) => (a, b, String, Char)
-----------------------------------------
--Typeclass:
-- Eq :Equality typeclass e.g. ==, /=, elem
-- Ord :Ordering typeclasses e.g. >, <, >=, <=

-- Show :Typeclass for converting a type to a string
-- ghci> show 7 -> "7"

-- Read :Typeclass for converting a string to a type
-- ghci> read "True" || False -> True

-- Num :Typeclass for numbers
-- ghci> :t 20 -> 20 :: Num a => a


--Typeclass is more like interface, we don't make data from typeclasses. Instead, we first make our [data type]
-- and then think about what it can act like.

--     Eq     Show     Read
--      │       │        │
--      └───┬───┘        └───▶ printable
--          │
--         Ord
--          │
--         Num
--      ┌───┴───────┐
--  Integral     Fractional

--      Functor
--         │
--    Applicative
--         │
--       Monad
