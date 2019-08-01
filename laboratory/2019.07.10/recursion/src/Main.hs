module Main where

import Prelude hiding (and, concat, replicate, (!!), elem)
import Data.List (sort)

-- Decide se todos os valores lógicos de uma lista são True.
and :: [Bool] -> Bool
and bs = foldr (&&) True bs

-- Concatena uma lista de listas.
concat :: [[a]] -> [a]
concat ess = foldr (++) [] ess

-- Produz uma lista com n valores idênticos.
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n e = e : replicate (n - 1) e

replicate2 :: Int -> a -> [a]
replicate2 n e = map (const e) [1 .. n]

-- Seleciona o enésimo elemento de uma lista.
(!!) :: [a] -> Int -> a
[] !! _ = error "Lista vazia"
(x : xs) !! p
  | p == 0 = x
  | otherwise = xs !! (p - 1)

-- Verifica se um valor é um elemento de uma lista.
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x : xs)
  | x == e = True
  | otherwise = elem e xs

-- Escreva o algoritmo de ordenação Selection Sort.
-- Teste seu algoritmo com o QuickCheck.
lessBetween :: Ord a => a -> a -> a
lessBetween a b
  | a < b = a
  | otherwise = b

listMinimum :: Ord a => [a] -> a
listMinimum (x : xs) = foldl lessBetween x xs

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst e (x : xs)
  | e == x = xs
  | otherwise = x : removeFirst e xs

selectionSort :: Ord a => [a] -> [a]
selectionSort xs = min' : selectionSort rest
  where
    min' = listMinimum xs
    rest = removeFirst min' xs

prop_selSortLength xs = length xs == length (selectionSort xs)

prop_selSortModel xs = sort xs == selectionSort xs

-- Defina uma função recursiva denominada merge que junta duas listas
-- ordenadas, resuntando em uma única lista ordenada.
merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge c@(x : xs) i@(y : ys)
  | x < y = x : merge xs i
  | otherwise = y : merge c ys

-- Sem utilizar funções já criadas para ordenação e usando a função
-- anterior, defina a função msort que implementa o algoritmo Merge
-- Sort seguindo as regras:
-- - Uma lista vazia ou um singleton já está ordenado.
-- - Qualquer outra lista é ordenada dividindo a lista em duas metades,
--   ordenando-as com msort e juntando com a função merge.
-- - Nota: crie uma função metade que divide uma lista ao meio.
mergeSort :: Ord a => [a] -> [a]
mergeSort xs = merge (mergeSort leftHalf) (mergeSort rightHalf)
  where
    (leftHalf, rightHalf) = splitAt (length xs `div` 2) xs

mergeSort2 :: Ord a => [a] -> [a]
mergeSort2 xs = mergeAll $ map (: []) xs
  where
    mergeAll :: Ord a => [[a]] -> [a]
    mergeAll [] = []
    mergeAll [y] = y
    mergeAll ys = mergeAll $ mergePairs ys

    mergePairs :: Ord a => [[a]] -> [[a]]
    mergePairs [] = []
    mergePairs [y] = [y]
    mergePairs (y0 : y1 : ys) = merge y0 y1 : mergePairs ys

main :: IO ()
main = do
  putStrLn "hello world"
