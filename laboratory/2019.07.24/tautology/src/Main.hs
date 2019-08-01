module Main where

import Data.List (nub)

data Prop = Const Bool      -- constante
          | Var Char        -- variável
          | Not Prop        -- Não
          | And Prop Prop   -- E
          | Imply Prop Prop -- Se-Então

type Subst = Assoc Char Bool

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

p1 :: Prop
p1 = (Var 'A') `And` (Not (Var 'A'))

p2 :: Prop
p2 = ((Var 'A') `And` (Var 'B')) `Imply` (Var 'A')

p3 :: Prop
p3 = (Var 'A') `Imply` ((Var 'A') `And` (Var 'B'))

p4 :: Prop
p4 = ((Var 'A') `And` ((Var 'A') `Imply` (Var 'B'))) `Imply` (Var 'B')

-- Retorna o resultado ao substituir as variáveis de uma proposição
-- por valores booleanos. Crie um pattern matching para cada possível
-- valor de Prop
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

-- Retorna a lista de variáveis em uma proposição.
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- Remove as variáveis duplicadas da lista.
uniquevars :: Prop -> [Char]
uniquevars p = nub (vars p)

-- Gera todas as combinações de valores True e False para um
-- certo número de variáveis.
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (\x -> False : x) bss ++ map (\x -> True : x) bss
  where
    bss = bools (n - 1)

-- Cria todos os mapas de substituição utilizando as listas
-- de substituições de bools.
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools $ length vs)
  where
    vs = uniquevars p

-- Verifica se a proposição é uma tautologia.
isTaut :: Prop -> Bool
isTaut p = all (`eval` p) (substs p)

main :: IO ()
main = do
  print $ isTaut p1
  print $ isTaut p2
  print $ isTaut p3
  print $ isTaut p4
