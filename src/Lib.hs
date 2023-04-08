module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

the :: (Num a, Show a) => a -> IO ()
the s = do
    let t = s * 2
    putStrLn $ "hi" ++ show t
