module Main where
 import System.Environment
 
 main :: IO ()
 main = do
 	 n <- getLine
 	 putStrLn ("Hello, " ++ n)