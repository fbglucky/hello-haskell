module Chapter5 where
import Data.List


funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "This is a string"

uncurriedFunction :: (Integer, Bool) -> IO ()
uncurriedFunction (i, b) = do
    print i
    print b

countChangesToMakeListIncrease :: [Int] -> Int
countChangesToMakeListIncrease [] = 0
countChangesToMakeListIncrease [x] = 0
countChangesToMakeListIncrease (x:xs) =
    if x >= head xs then (x - head xs) + 1 + countChangesToMakeListIncrease ((x + 1) : tail xs) else countChangesToMakeListIncrease xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

isPalindrome' []  = True
isPalindrome' [_] = True
isPalindrome' xs  = head xs == last xs && isPalindrome' (init $ tail xs)

palindromeRearranging :: (Eq a) => [a] -> Bool
palindromeRearranging [] = True
palindromeRearranging (x: xs) =
    if x `elem` xs then palindromeRearranging(delete x xs)
    else (rem (length xs) 2 == 0) && palindromeRearranging xs

