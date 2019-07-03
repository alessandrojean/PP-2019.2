module Caesar where
import Data.Char

-- Converte uma letra minúscula para inteiro.
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- Converte um inteiro para letra minúscula.
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- Retorna a n-ésima letra seguinte,
-- evite ultrapassar o limite com `mod` 26.
shift :: Int -> Char -> Char
shift d c
  | isLetter c && isLower c = int2let ((d + let2int c) `mod` 26)
  | otherwise = c

-- Aplica a função shift em cada letra da String.
encode :: Int -> String -> String
encode d s = [shift d x | x <- s]

-- Efetua o crack da String encodada.
crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

-- Quantidade de letras minúsculas em uma String.
lowers :: String -> Int
lowers s = length [x | x <- s, isLetter x && isLower x]

-- Conta a ocorrência de um caracter em uma String,
count :: Char -> String -> Int
count c s = length [x | x <- s, x == c]

-- Dado um n e m, calcule 100 * n / m
percent :: Int -> Int -> Float
percent n m = 100.0 * (fromIntegral n / fromIntegral m)

-- Calcule a porcentagem de cada letra minúscula
-- do alfabeto em uma String.
-- A porcentagem é a contagem de ocorrência pelo total
-- de letras minúsculas.
freqs :: String -> [Float]
freqs s = [percent (count c s) (lowers s) | c <- ['a' .. 'z']]

-- Calcule a medida de Chi-Quadrado de duas
-- tabelas de frequência:
-- Soma (Observado - Esperado) ^ 2 / Esperado
chisqr :: [Float] -> [Float] -> Float
chisqr t1 t2 = sum [(obs - esp) ^ 2 / esp | (obs, esp) <- zip t1 t2]

-- Rotaciona uma tabela em n posições.
rotate :: Int -> [a] -> [a]
rotate p es = drop p es ++ take p es

-- Retorna a lista das posições que contém um
-- elemento x.
positions :: Eq a => a -> [a] -> [Int]
positions x es = [i | (x', i) <- zip es [0..], x == x']
