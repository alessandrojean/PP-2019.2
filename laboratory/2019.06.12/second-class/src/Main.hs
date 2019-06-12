module Main where

mySum :: (Int, Int) -> Int
mySum (x, y) = x + y

zeroUntilN :: Int -> [Int]
zeroUntilN n = [0..n]

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1, 2, 3], [4, 5, 6]]

soma :: Int -> Int -> Int -> Int
soma a b c = a + b + c

copia :: a -> (a, a)
copia x = (x, x)

f :: a -> a
f a = a

g :: Eq a => a -> (a, a) -> Bool
g x (y, z) = (x == y) || (x == z)

h :: Num a => Int -> a -> a
h x y = fromIntegral x * y

main :: IO ()
main = do
  print (mySum (1, 2))
  print (zeroUntilN 5)
