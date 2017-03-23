module Main where
import Lib
import System.IO (readFile)

main :: IO ()
main = do
  putStrLn (greet "bobby")
  putStrLn (greet "World")


printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents



