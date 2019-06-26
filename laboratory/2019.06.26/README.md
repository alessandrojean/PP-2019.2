# Funções em Haskell
*Retirado do roteiro de aula do Prof. Emilio Francesquini, CMCC/UFABC.*

## Funções

O Haskell é baseado no cálculo λ. Como vimos na aula passada, toda
função recebe uma entrada e uma saída:

```hs
somaUm :: Integer -> Integer
somaUm x = x + 1
```

### Mais de uma variável

Funções de mais de uma variável, na verdade são composições de funções:

```hs
soma x y = x + y
```

na verdade é:

```hs
-- Resultado de soma x aplicado em y
soma :: Integer -> (Integer -> Integer)
soma x y = (soma x) y
soma = \x -> (\y -> x + y)
```

Isso é denominado *currying*.

### Exercício 1

Qual a única função possível com a seguinte assinatura?

```hs
f :: a -> a
```

### Exemplos de funções

Funções devem ser escritas em forma de expressões combinando outras funções,
de tal forma a manter simplicidade:

```hs
impar :: Integral a => a -> Bool
impar n = n `mod` 2 == 1

quadrado :: Num a => a -> a
quadrado n = n * n

quadradoMais6Mod9 :: Integral a => a -> a
quadradoMais6Mod9 n = (n * n + 6) `mod` 9
```

### Exercício 2

Escreva uma função que retorne as raizes de uma equação do segundo grau:

```hs
raiz2Grau :: Floating a => a -> a -> a -> (a, a)
raiz2Grau a b c = ( ???, ??? )
```

Teste com `raiz2Grau 4 3 (-5)` e `raiz2Grau 4 3 5`.

### Cláusula `where`

Para organizar nosso código, podemos utilizar a cláusula `where` para
definir nomes intermediários:

```hs
f x = y + z
  where
    y = e1
    z = e2
```

- Atenção à indentação!

### Exemplo

```hs
euclidiana :: Floating a => a -> a -> a
euclidiana x y = sqrt diffSq
  where
    diffSq = diff ^ 2
    diff = x - y
```

### Exercício 3

Reescreva a função `raiz2Grau` utilizando `where`.

### Condicionais

A função `if-then-else` nos permite utilizar desvios condicionais em nossas
funções:

```hs
abs :: Num a => a -> a
abs n = if (n >= 0) then n else (-n)
```

ou

```hs
abs :: Num a => a -> a
abs n = if (n >= 0)
        then n
        else (-n)
```

Também podemos encadear condicionais:

```hs
signum :: (Ord a, Num a) => a -> a
signum n = if (n == 0)
           then 0
           else if (n > 0)
                then 1
                else (-1)
```

### Exercício 4

Utilizando condicionais, reescreva a função `raiz2Grau` para retornar
`(0, 0)` no caso de delta negativo.

Note que a assinatura da função agora deve ser:

```hs
raiz2Grau :: (Ord a, Floating a) => a -> a -> a -> (a, a)
```

- *Por quê?*

### Expressões *guardadas* (Guard Expressions)

Uma alternativa ao uso de `if-then-else` é o uso de *guards* (`|`) que
deve ser lido como *tal que*:

```hs
signum :: (Ord a, Num a) => a -> a
signum n | n == 0 = 0     -- signum n tal que n == 0
                          -- é definido como 0
         | n > 0 = 1
         | otherwise = -1
```

- Expressões imediatamente à direita de `|` devem ser do tipo `Bool`
- Caso mais de uma seja `True`, **apenas a primeira** delas
  será executada.
- `otherwise` é o caso contrário e é definido como `otherwise = True`.

Note que expressões guardadas são avaliadas de cima para baixo, o primeiro
verdadeiro será executado e o restante ignorado.

```hs
classificaIMC :: Double -> String
classificaIMD img
  | imc <= 18.5 = "abaixo do peso"
  -- não preciso fazer && imc > 18.5
  | imc <= 25.0 = "no peso correto"
  | imc <= 30.0 = "acima do peso"
  | otherwise = "muito acima do peso"
```

### Exercício 5

Utilizando guards, reescreva a função `raiz2Grau` para retornar um
erro com raízes negativas.

Para isso utilize a função `error`:

```hs
error "Raízes negativas."
```

### Nota sobre erro

O uso de `error` interrompe a execução do programa. Nem sempre é a 
melhor forma de tratar erro, aprenderemos alternativas ao longo do curso.

## Pattern Matching

Considere a seguinte função:

```hs
not :: Bool -> Bool
not x = if (x == TRUE) then FALSE else TRUE
```

Podemos reescreve-la utilizando guardas:

```hs
not :: Bool -> Bool
not x | x == TRUE = FALSE
      | x == FALSE = TRUE
```

Quando temos comparações de igualdade nos guardas, podemos definir as
expressões substituindo diretamente os argumentos:

```hs
not :: Bool -> Bool
not TRUE = FALSE
not FALSE = TRUE
```

Não precisamos enumerar todos os casos, podemos definir apenas casos
especiais:

```hs
soma :: (Eq a, Num a) => a -> a -> a
soma x 0 = x
soma 0 y = y
soma x y = x + y
```

Assim como os guards, os padrões são avaliados em sequência do primeiro
até o último.

### Exercício 6 - Pattern Matching

Implemente a multiplicação utilizando Pattern Matching:

```hs
mul :: Num a => a -> a -> a
mul x y = x * y
```

### Pattern Matching

Quando a saída não depende da entrada, podemos substituir a entrada
por `_` (lê-se "não importa"):

```hs
mul :: (Eq a, Num a) => a-> a -> a
mul 0 _ = 0
mul _ 0 = 0
mul x 1 = x
mul 1 y = y
mul x y = x * y
```

Como o Haskell é preguiçoso, ao identificar um padrão contendo `0` ele
não avaliará o outro argumento.

### Expressões λ

As expressões lambdas feitas anteriormente são válidas no Haskell.
Vamos criar uma função que retorna uma função que soma e multiplica
`x` a um certo número:

```hs
somaMultX :: a -> (a -> a)
somaMultX x = \y -> x + x * y

somaMult2 = somaMultX 2
```

### Operadores

- Operadores nada mais são do que funções cujos nomes são compostos por 
  símbolos e não letras.
- Para definir um operador em Haskell, podemos criar na forma infixa
  ou na forma de função.

```hs
(:+) :: Num a => a -> a -> a
x :+ y = abs x + y
```

ou

```hs
(:+) :: Num a => a -> a -> a
(:+) x y = abs x + y
```

Da mesma forma, uma função pode ser utilizada como operador se envolta
de crases:

```console
> mod 10 3
1
> 10 `mod` 3
1
```

Sendo `#` um operador, temos que `(#)`, `(x #)`, `(# y)` são chamados
de seções, e definem:

```hs
(#) = \x -> (\y -> x # y)
(x #) = \y -> x # y
(# y) = \x -> x # y
```

Essas formas são também conhecidas como *point-free notation*:

```console
> (/) 4 2
2
> (/2) 4
2
> (4/) 2
2
```

### Exercício 7

Considere o operador `(&&)`, simplifique a definição para apenas dois
padrões:

```hs
(&&) :: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False
```

### Exercício 8

Os seus malvados favoritos professores de Processamento da Informação 
resolveram em uma reunião normatizar a quantidade de avaliações, a 
conversão nota-conceito e a definição do conceito final dados os 
conceitos de Teoria e Prática:

- As avaliações de consistirão de duas provas na Teoria e duas na Prática.
- A nota final de cada turma será dada pela média ponderada das provas, 
  o peso é determinado por cada professor.
- A conversão conceito final segue a seguinte tabela:
  - nota < 5 = F
  - nota < 6 = D
  - nota < 7 = C
  - nota < 8 = B
  - nota >= 8 = A
- O conceito final é dado pela seguinte tabela:

  | Teoria | Prática | Final |
  | ------ | ------- | ----- |
  | A      | A       | A     |
  | A      | B       | A     |
  | A      | C       | B     |
  | A      | D       | B     |
  | A      | F       | F     |
  | B      | A       | B     |
  | B      | B       | B     |
  | B      | C       | B     |
  | B      | D       | C     |
  | B      | F       | F     |
  | C      | A       | B     |
  | C      | B       | C     |
  | C      | C       | C     |
  | C      | D       | C     |
  | C      | F       | F     |
  | D      | A       | C     |
  | D      | B       | C     |
  | D      | C       | D     |
  | D      | D       | D     |
  | D      | F       | F     |
  | F      | A       | F     |
  | F      | B       | F     |
  | F      | C       | F     |
  | F      | D       | F     |
  | F      | F       | F     |

Criem as seguintes funções para ajudar os professores a calcularem o
conceito final de cada aluno:

```hs
-- dados dois pesos w1, w2, retorna uma função que
-- calcula a média ponderada de p1, p2
mediaPonderada :: (Eq a, Floating a) =>
a -> a -> (a -> a -> a)
5
-- converte uma nota final em conceito
converteNota :: (Ord a, Floating a) => a -> Char
8
-- calcula conceito final
conceitoFinal :: Char -> Char -> Char
```

A função `mediaPonderada` deve dar erro se `w1 + w2 /= 1.0`.

Acrescente o seguinte código ao final do arquivo:

```hs
turmaA1Pratica = mediaPonderada 0.4 0.6
turmaA1Teoria = mediaPonderada 0.3 0.7

p1A1P = 3
p2A1P = 8
p1A1T = 7
p2A1T = 10

mediaP = turmaA1Pratica p1A1P p2A1P
mediaT = turmaA1Teoria p1A1T p2A1T

finalA1 = conceitoFinal
(converteNota mediaP) (converteNota mediaT)

turmaA2Pratica = mediaPonderada 0.4 0.6
turmaA2Teoria = mediaPonderada 0.3 0.9

p1A2P = 3
p2A2P = 8
p1A2T = 7
p2A2T = 10

mediaA2P = turmaA2Pratica p1A2P p2A2P
mediaA2T = turmaA2Teoria p1A2T p2A2T

finalA2 = conceitoFinal
(converteNota mediaA2P) (converteNota mediaA2T)
```

Teste os códigos com `print finalA1` e `print finalA2`.