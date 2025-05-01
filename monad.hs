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

        