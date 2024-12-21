
-- Functions for I/O -------------------------------

-- 'putStrLn'
-- 'putStr' 
-- they take a string and return an I/O action that will print the string to the console
-- 'putStr' doesn't jump into a new line after printing the string while 'putStrLn' does

-- 'putChar'
-- takes a character and return an I/O action that will print it out to the terminal

-- 'putStr' is actually defined recursively with 'putChar':

putStr' :: String -> IO()
putStr' [] = return()
putStr' (x:xs) = do
    putChar x
    putStr' xs

-- 'print' takes a value of any type that's an instance of 'Show'. 
-- it first runs 'show' on a value and then feed it to 'putStrLn'
-- so print <=> putStrLn . show


main = do 
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return()

-- PS D:\__Haskell_proj__> runhaskell Input_Output3.hs
-- Computer Science      -- input
-- Computer              -- output

-- Due to buffering, the execution of program will begin only we hit the 'Enter' key
-- not after every inputted character.

getLine' :: IO String
getLine' = do
    x <- getChar
    if x == '\n' then return()
    else do
        xs <- getLine'
        return (x:xs)