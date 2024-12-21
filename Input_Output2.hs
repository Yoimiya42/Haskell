

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-- 'words' takes a string  and return a list of words
-- 'unwords' takes a list of words and return a string

main :: IO ()
main = do 
    line <- getLine
    if null line
        then return ()  -- return an empty I/O action
        else do 
            putStrLn $ reverseWords line
            main  -- call 'main' recursively

-- return() doesn't cause the do-block to end in execution


main2 = do
    a <- return "Hello"
    b <- return "World"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 7
    putStrLn line

-- All these return done nothing, just encapsulated the results 
-- and throw them away cause they are not bound to a any name

main3 = do
    a <- return "Alogorithms"
    b <- return "Data Structures"
    putStrLn $ b ++ " and " ++ a

-- return is used to wrap a value into an I/O action
-- '<-' is used to bind the result of an I/O action to a name
-- return is opposite of '<-' 

-- so the operations above are redundant and equivalent to:
-- let a = "Algorithms"
-- let b = "Data Structures"

-- The final result of a do-block is determined by the last I/O action
-- Mostly, when we create an I/O action that doesn't do anything or 
-- don't want the I/O action have the result of the last I/O action

-- but we use 'return' to make an I/O action has our desired result