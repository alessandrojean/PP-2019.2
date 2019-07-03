module Main where

import Data.List
import Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (<= x) xs
    rhs = filter (> x) xs

prop_idempotencia :: Ord a => [a] -> Bool
prop_idempotencia xs = qsort (qsort xs) == qsort xs

prop_length :: Ord a => [a] -> Bool
prop_length xs = length (qsort xs) == length xs

prop_minimum :: Ord a => [a] -> Property
prop_minimum xs = not (null xs) 
  ==> head (qsort xs) == minimum xs

prop_model :: Ord a => [a] -> Bool
prop_model xs = qsort xs == sort xs

par x = x `mod` 2 == 0

prop_alternanciaParImpar :: Integral a => a -> Bool
prop_alternanciaParImpar n = par n /= par (n + 1)

impar x = x `rem` 2 == 1

prop_seImparNaoPar n = par n ==> not (impar n)

fatorial:: Integral a => a -> a
fatorial n
  | n == 0 = 1
  | otherwise = n * fatorial (n - 1)

prop_fatorialNFatorialNMaisUm (NonNegative n) =
  fatorial n * (n + 1) == fatorial (n + 1)

collatz :: Integral a => a -> a
collatz 1 = 1
collatz n
  | par n = collatz (n `div` 2)
  | otherwise = collatz (3 * n + 1)

prop_collatz (Positive n) = collatz n == 1

main :: IO ()
main = do
  putStrLn "hello world"
