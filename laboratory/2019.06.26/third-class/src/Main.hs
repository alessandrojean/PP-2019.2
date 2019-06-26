module Main where

f :: a -> a
f a = a

raiz2Grau :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2Grau a b c 
  | delta >= 0 = (x1, x2)
  | otherwise = error "Raízes negativas."
  where
    x1 = ((-b) + sqDelta) / (2 * a)
    x2 = ((-b) - sqDelta) / (2 * a)
    sqDelta = sqrt delta
    delta = b ^ 2 - 4 * a * c

mul :: (Eq a, Num a) => a -> a -> a
mul 0 _ = 0
mul _ 0 = 0
mul x 1 = x
mul 1 y = y
mul x y = x * y

(+-) :: Bool -> Bool -> Bool
True +- True = True
_ +- _ = False

-- Dados dois pesos w1, w2, retorna uma função que
-- calcula a média ponderada de p1, p2
mediaPonderada :: (Eq a, Floating a) => a -> a -> (a -> a -> a)
mediaPonderada w1 w2
  | sumW == 1.0 = \p1 p2 -> w1 * p1 + w2 * p2
  | otherwise = error "Média diferente de 1."
  where
    sumW = w1 + w2

-- Converte uma nota final em conceito.
converteNota :: (Ord a, Floating a) => a -> Char
converteNota n
  | n < 5 = 'F'
  | n < 6 = 'D'
  | n < 7 = 'C'
  | n < 8 = 'B'
  | otherwise = 'A'

-- Calcula conceito final.
conceitoFinal :: Char -> Char -> Char
conceitoFinal 'F' _ = 'F'
conceitoFinal _ 'F' = 'F'
conceitoFinal 'D' 'D' = 'D'
conceitoFinal 'D' 'C' = 'D'
conceitoFinal 'D' _ = 'C'
conceitoFinal 'C' 'A' = 'B'
conceitoFinal 'C' _ = 'C'
conceitoFinal 'B' 'D' = 'C'
conceitoFinal 'B' _ = 'B'
conceitoFinal 'A' 'C' = 'B'
conceitoFinal 'A' 'D' = 'B'
conceitoFinal 'A' _ = 'A'

turmaA1Pratica = mediaPonderada 0.4 0.6
turmaA1Teoria = mediaPonderada 0.3 0.7

p1A1P = 3
p2A1P = 8
p1A1T = 7
p2A1T = 10

mediaP = turmaA1Pratica p1A1P p2A1P
mediaT = turmaA1Teoria p1A1T p2A1T

finalA1 = conceitoFinal (converteNota mediaP) (converteNota mediaT)

turmaA2Pratica = mediaPonderada 0.4 0.6
turmaA2Teoria = mediaPonderada 0.3 0.9

p1A2P = 3
p2A2P = 8
p1A2T = 7
p2A2T = 10

mediaA2P = turmaA2Pratica p1A2P p2A2P
mediaA2T = turmaA2Teoria p1A2T p2A2T

finalA2 = conceitoFinal (converteNota mediaA2P) (converteNota mediaA2T)

main :: IO ()
main = do
  print finalA1
  print finalA2
