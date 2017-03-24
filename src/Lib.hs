module Lib where
import Math
import System.Random (randomRIO)

welcome :: IO ()
welcome = putStrLn "Welcome to Haskell!"

sqr :: Int -> Int
sqr x = x * x

greet name = "Hello " ++ name ++ "!"

data Mood = Blah | Woot deriving Show
changeMood Blah = Woot
changeMood Woot = Blah

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool coolness
        then putStrLn "eyyyyy. What's shakin'?"
    else
        putStrLn "pshhhh."
    where cool v = v == "downright frosty yo"


qsort [] = []
qsort (x: xs) = qsort lows ++ (x: qsort highs)
    where lows = filter (<= x) xs
          highs = filter (> x) xs





