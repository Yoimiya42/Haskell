


-- Type the following at the command prompt:
-- ghc Input_Output.hs
-- [1 of 2] Compiling Main             ( Input_Output.hs, Input_Output.o )
-- [2 of 2] Linking Input_Output.exe

-- ./Input_Output
-- Hello,World!

-- ghci> :t putStrLn
-- putStrLn :: String -> IO ()
-- ghci> :t putStrLn "Hello World"
-- putStrLn "Hello World" :: IO ()

-- putStrLn takes a [string] and return an [I/O action] has a type of () -- the empty tuple or unit



main = do  
    putStrLn "Hello, what's your name?"
    name <- getLine   
    -- getLine is an I/O action that reads a line from the console and binds it to 'name'
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- ghvi> :t getLine
-- getLine :: IO String

-- I/O actions will only be performed when they are given a name of 'main' or inside a 'do' block
-- also, when we type out an I/O action in GHCI, it will be performed immediately. 
-- cause 'show' is called on it and putStrLn is implicitly called.