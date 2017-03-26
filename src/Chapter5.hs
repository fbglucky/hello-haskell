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

tryFun :: a -> a -> a
tryFun x y = y
--tryFun x y = x

tryFun2 :: a -> b -> b
tryFun2 x y = y

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if x > y then True else False -- should be x > y

functionS :: (a, b) -> b
functionS (x, y) = y

id :: a -> a
id x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
--r x = tail x
--r x = init x
r x = x ++ x

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

a:: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x

fstString :: String -> String
fstString x = x ++ " in the rain"

sndString :: String -> String
sndString x = x ++ " over the rainbow"

sing = if x < y then fstString x else sndString y
  where x = "Singin"; y = "Somewhere"


main :: IO ()
main = do
  print (1 + 2)
  print blah
  print $ negate (-1)
  print $ (+) 0 blah
  where blah = negate 1

data Woot
data Blah

f1 :: Woot -> Blah
f1 = undefined

g1 :: (Blah, Woot) -> (Blah, Blah)
g1 (b, w) = (b, f1 w)

f2 :: Int -> String
f2 = undefined

g2 :: String -> Char
g2 = undefined

h2:: Int -> Char
h2 x = g2 $ f2 x

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst $ g $ f x