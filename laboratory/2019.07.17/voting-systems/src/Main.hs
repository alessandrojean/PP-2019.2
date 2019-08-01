module Main where

import Data.List
import Test.QuickCheck

votes :: [String]
votes = ["Vermelho", "Azul", "Verde", "Azul", "Azul", "Vermelho"]

rankVotes :: [[String]]
rankVotes = [["Vermelho", "Verde"],
             ["Azul"],
             ["Verde", "Vermelho", "Azul"],
             ["Azul", "Verde", "Vermelho"],
             ["Verde"]]

-- Conta quantos votos o candidato x recebeu.
count :: Eq a => a -> [a] -> Int
count x vs = length $ filter (== x) vs

-- Retorna a lista de elementos únicos.
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs)
  | x `elem` xs = unique xs
  | otherwise = x : unique xs

-- Retorna uma lista de pares ordenados (votos, candidato) com o total
-- de votos obtido por cada candidato. Use a função sort para ordenar
-- do menos para o mais votado.
result :: Ord a => [a] -> [(Int, a)]
result vs = sort $ map (\x -> (count x vs, x)) $ unique vs

-- Retorna o vencedor da eleição.
winner :: Ord a => [a] -> a
winner vs = (snd . last) (result vs)

-- Elimina as listas vazias de uma lista de listas
rmEmpty :: Eq a => [[a]] -> [[a]]
rmEmpty = filter (not . null)

-- Elimina um candidado da lista de votos.
remove :: Eq a => a -> [[a]] -> [[a]]
remove x vss = map (filter (/= x)) vss

-- Retorna uma lista dos candidados existentes, do menos para o mais votado.
rank :: Ord a => [[a]] -> [a]
rank vss = map snd $ result $ map head vss

-- Retorna o vencedor executando o processo descrito acima.
winner' :: Ord a => [[a]] -> a
winner' vss
  | length rankNoEmpty == 1 = head rankNoEmpty
  | otherwise = winner' (remove (head rankNoEmpty) vss)
  where
    rankNoEmpty = rank (rmEmpty vss)

prop_isSorted :: Ord a => [a] -> Property
prop_isSorted vs = not (null vs)
  ==> all (\(x1, x2) -> x1 <= x2) $ zip res (tail res)
  where
    res = result vs

prop_dontHaveNull :: Eq a => [[a]] -> Property
prop_dontHaveNull vss = not (null vss)
  ==> not (any null (rmEmpty vss))

main :: IO ()
main = do
  print $ winner votes
  print $ winner' rankVotes
