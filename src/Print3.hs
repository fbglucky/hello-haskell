module Print3 where

myGreeting :: String
myGreeting = "hello" ++ "world!"

hello :: String
hello = "Hello"

world :: String
world = "WOrld!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = concat [hello, " ", world]
