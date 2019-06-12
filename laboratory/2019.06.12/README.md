# Tipos e Classes
*Retirado do roteiro de aula do Prof. Emilio Francesquini, CMCC/UFABC.*

## Tipos e Classes Padrões

### Tipos de dados

- Um **tipo** é uma coleção de valores relacionados entre si.

> **Exemplos**
> - `Int` compreende todos os valores de números inteiros.
> - `Bool` contém apenas os valores `True` e `False`, representando
>   valores lógicos.

- Em Haskell, os tipos são definidos pela notação

  ```hs
  v :: T
  ```

- Significando que `v` define um valor do tipo `T`.

  ```hs
  False :: Bool
  True :: Bool
  10 :: Int
  ```

### Tipos de funções

- De forma similar uma função pode ser definida por
  
  ```hs
  f :: T0 -> T1
  ```

- Indicando que a função `f` recebe um valor do tipo `T0` e retorna
  um valor do tipo `T1`.

### Tipos avaliados

- O tipo da aplicação de uma função é o tipo do seu retorno:

  ```hs
  False :: Bool
  not :: Bool -> Bool
  not False :: Bool
  ```

### Inferência de tipo

- Em Haskell, **toda** expressão tem um tipo calculado antes de avaliar
  o resultado da expressão.
- Os tipos podem ser definidos automaticamente pela inferência do tipo.

Por exemplo, se eu tenho:

```hs
f :: A -> B
e :: A
```

então

```hs
f e :: B
```

### Exemplo

```hs
impar x = x `rem` 2 == 1
```

> **Pergunta**
>
> Qual o tipo da função?

Abra o `ghci` e digite:

```console
> :t (`rem` 2)
(`rem` 2) :: Integral a => a -> a
```

Logo `x` deve ser do tipo `Integral` e a função deve ser:

```hs
impar :: Integral a => a -> ???
impar x = x `rem` 2 == 1
```

```console
> :t (== 1)
(== 1) :: (Eq a, Num a) => a -> Bool
```

Isso restringe ainda mais nosso tipo, como veremos mais a frente.
Por ora, observemos `-> Bool`.

A assinatura da função fica:

```hs
impar :: Integral a => a -> Bool
impar x = x `rem` 2 == 1
```

Se eu fizer (ou tentar):

```hs
r1 = impar "3"
```

Isso vai gerar um **erro de compilação**!

```console
No instance for (Integral [Char]) arising from a use of 'impar'
In the expression: impar "3"
In an equation for 'r1': r1 = impar "3"
```

## Tipos Básicos

- O compilador GHC já vem com suporte nativo a diversos tipos básicos.
- Durante o curso veremos como definir e criar os nossos próprios tipos.

Os tipos são:

- `Bool`: contém os valores `True` e `False`. Expressões booleanas podem
  ser executadas com os operadores `&&` (e), `||` (ou) e `not`.
- `Char`: contém todos os caracteres no sistema *Unicode*. Podemos representar
  a letra `'a'`, o número `'5'`, a seta tripla `'⇶'` e o *homem de terno
  levitando*<sup>1</sup> `'🕴️'`.
- `String`: sequências de caracteres delimitados por aspas duplas:
  `"Olá Mundo"`.
- `Int`: inteiros com precisão fixa em 64 bits. Representa os valores
  numéricos de -2<sup>63</sup> até 2<sup>63</sup> - 1.
- `Integer`: inteiros de precisão arbitrária. Representa valores inteiros
  de qualquer precisão, a memória é o limite. Mais lento do que operações
  com `Int`.
- `Float`: valores em ponto-flutuante de precisão simples. Permite representar
  números com um total de 7 dígitos, em média.
- `Double`: valores em ponto-flutuante de precisão dupla. Permite representar
  números com quase 16 dígitos, em média.

<sup>1</sup> Este é o nome oficial do caracter na tabela Unicode v7.0!

Note que ao escrever:

```hs
x = 3
```

O tipo de `x` pode ser `Int`, `Integer`, `Float` ou `Double`.

> **Pergunta**
>
> Qual tipo devemos atribuir a `x`?

### Listas

*Listas* são sequências de elementos do mesmo tipo agrupados por colchetes
e separados por vírgula.

Uma lista de tipo `T` tem tipo `[T]`:

```hs
[1, 2, 3, 4] :: Int
[False, True, True] :: [Bool]
['o', 'l', 'a'] :: [Char]
```

- O *tamanho* da lista (*length*) representa a quantidade de elementos
  que ela contém.
- Uma lista vazia é representada por `[]`.
- Listas com um elemento, como `[1]`, `[False]` e `[[]]` são chamadas
  *singleton*.
- Como podem ter percebido, podemos ter listas de listas:

  ```hs
  [ [1, 2, 3], [4, 5] ] :: [[Int]]
  [ ['o', 'l', 'a'], ['m', 'u', 'n', 'd', 'o'] ] :: [[Char]]
  ```

Notem que:
- O tipo da lista não especifica seu tamanho.
- Não existe limitação quanto ao tipo da lista.
- Não existe limitação quanto ao tamanho da lista.

### Tuplas

- *Tuplas* são sequências finitas de componentes, contendo zero ou
  mais tipos diferentes:

  ```hs
  (True, False) :: (Bool, Bool)
  (1.0, "Sim", False) :: (Double, String, Bool)
  ```
- O tipo da tupla é definido como `(T1, T2, ..., Tn)`.
- O número de componentes de uma tupla é chamado *aridade* (*arity*).
- Uma tupla de aridade zero, a tupla vazia, é representada por `()`.
- Tuplas de tamanho dois são conhecidas como *duplas*, já as de tamanho
  três são *triplas*.

Notem que:
- O tipo da tupla especifica seu tamanho.
- Não existe limitações aos tipos associados à tupla (podemos ter tuplas
  de tuplas).
- Tuplas **devem** ter um tamanho finito (e fixo!).
- Tuplas de aridade 1 não são permitidas para manter compatibilidade 
  do uso de parênteses para definir a ordem de avaliação.

### Funções

- *Funções* são mapas de argumentos de um tipo para resultados em outro
  tipo. O tipo de uma função é escrita como `T1 -> T2`, ou seja, o mapa
  do tipo `T1` para o tipo `T2`:

  ```hs
  not :: Bool -> Bool
  even :: Int -> Bool
  ```
- Como não existem restrições para os tipos, a noção de mapa de um tipo
  para outro é suficiente para escrever funções com 0 ou mais argumentos
  e que retornem 0 ou mais valores.

> **Exercício**
>
> Crie as seguintes funções em um arquivo `aula02.hs`, carregue no `ghci`,
> verifique seu tipo e teste com algumas entradas:
>
> ```hs
> soma :: (Int, Int) -> Int
> soma (x, y) = x + y
>
> zeroAteN :: Int -> [Int]
> zeroAteN n = [0..n]
> ```

- Uma função pode ser *total* se ela for definida para qualquer valor do
  tipo de entrada ou *parcial* se existem algumas entradas para qual ela
  não tem valor de saída definida:

  ```console
  > head []
  *** Exception: Prelude.head: empty list
  ```

### Curry

- É óbvio que o pessoal do Unicode também criopu um símbolo para o curry
  japonês (karê, カレー): :curry:.

  ```console
  Prelude> :t '🍛'
  '🍛' :: Char
  ```

- Funções com múltiplos argumentos podem ser definidas de uma outra forma,
  inicialmente não óbvia, mas que torna sua representação mais natural.

Como não existem restrições de tipos, uma função pode retornar uma outra
função:

```hs
soma' :: Int -> (Int -> Int)
soma' x = \y -> x + y
```

- Ela recebe um valor `x` e retorna uma função que recebe um `y` e devolve
  `y + x` (aprenderemos sobre `\y` mais adiante).

A seguinte definição ainda é válida:

```hs
soma' :: Int -> (Int -> Int)
soma' x y = x + y
```

Ela indica que a função `soma'` recebe um valor `x`, cria uma função
`\y -> x + y` e aplica com o valor `y`. Isso é conhecido como *curried
functions*.

Da mesma forma podemos ter:

```hs
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z
```

Para evitar escrever um monte de parênteses (como no Lisp), a seguinte
sintaxe é válida:

```hs
soma' :: Int -> Int -> Int
soma' x y = x + y

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z
```

## Polimorfismo

### Tipos polimórficos

Considere a função `length` que retorna o tamanho de uma lista.
Ela deve funcionar para qualquer uma dessas listas:

```hs
[1, 2, 3, 4] :: [Int]
[False, True, True] :: [Bool]
['o', 'l', 'a'] :: [Char]
```

> **Pergunta**
>
> Qual é então o tipo de `length`?

```hs
length :: [a] -> Int
```

- Quem é `a`?
- Em Haskell, `a` é conhecida como *variável de tipo* e ela indica que
  a função deve funcionar para listas de qualquer tipo.
- As variáveis de tipo devem seguir a mesma convenção de nomes do Haskell,
  iniciar com letra minúscula. Como convenção utilizamos `a`, `b`, ...

### Overloaded types

- Considere agora a função `(+)`, diferente de `length` ela pode ter um
  comportamento diferente para tipos diferentes.
- Internamente somar dois `Int` pode ser diferente de somar dois `Integer`
  (e definitivamente é diferente de somar dois `Float`).
- Ainda assim essa função *deve* ser aplicada a tipos *numéricos*.
- A ideia de que uma função pode ser aplicada a apenas uma classe de tipos
  é explicitada pela *Restrição de classe* (*class constraint*).
- Uma restrição é escrita na forma `C a`, onde `C` é o nome da classe
  e `a` uma variável de tipo.

  ```hs
  (+) :: Num a => a -> a -> a
  ```
- A função `+` recebe dois tipos de uma classe numérica e retorna um valor
  desse *mesmo tipo*.
- Note que nesse caso, ao especificar a entrada como `Int` para o primeiro
  argumento, todos os outros *devem* ser `Int` também.
- Uma vez que uma função contém uma restrição de classe, pode ser necessário
  definir *instâncias* dessa função para diferentes tipos pertencentes
  à classe.
- Os valores também podem ter restrição de classe:
  
  ```hs
  3 :: Num a => a
  ```

### Classes de tipos

Lembrando:
- *Tipo*: coleção de valores relacionados.
- *Classe*: coleção de tipos que suportam certas funções ou operadores.
- *Métodos*: funções requisitos de uma classe.

### `Eq` - classe da igualdade

- Tipos que podem ser comparados em igualdade e desigualdade:

  ```hs
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  ```

```console
> 1 == 2
False
> [1, 2, 3] == [1, 2, 3]
True
> "Ola" /= "Alo"
True
```

### `Ord` - classe de ordem

- A classe `Eq` acrescida de operadores de ordem:

  ```hs
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  min :: a -> a -> a
  max :: a -> a -> a
  ```

```console
> 4 < 6
> min 5 0
> max 'c' 'h'
> "Ola" <= "Olaf"
```

### `Show` - classes imprimíveis

- A classe `Show` define como imprimir um valor de um tipo:

  ```hs
  show :: a -> String
  ```

```console
> show 10.0
> show [1, 2, 3, 4]
```

### `Read` - classe legíveis

- A classe `Read` define como ler um valor de uma `String`:

  ```hs
  read :: String -> a
  ```
- Precisamos especificar o tipo que queremos extrair da `String`:
  
  ```console
  > read "12.5" :: Double
  > read "False" :: Bool
  > read "[1, 3, 4]" :: [Int]
  ```

### `Num` - classe numérica

- A classe `Num` define todos os tipos numéricos e deve suas instâncias
  devem responder à:

  ```hs
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  ```

```console
> 1 + 3
> 6 - 9
> 12.3 * 5.6
```

- O que as seguintes funções fazem? (use o `:t` para ajudar)

  ```console
  > negate 2
  > abs 6
  > signum (-1)
  > fromInteger 3
  ```
- `negate`: inverte o sinal do argumento.
- `abs`: retorna o valor absoluto.
- `signum` retorna o sinal do argumento.
- `fromInteger`: converte um argumento do tipo inteiro para numérico.

Note que os valores negativos devem ser escritos entre parênteses para
não confundir com o operador de subtração.

### `Integral` - classe de números inteiros

- A classe `Integral` define todos os tipos numéricos inteiros e suas
  instâncias devem responder a:

  ```hs
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  ```

- O uso de crases transforma uma função em operador infixo.

  ```console
  > quot 10 3 == 10 `quot` 3
  ```

```console
> 10 `quot` 3
> 10 `rem` 3
> 10 `div` 3
> 10 `mod` 3
```

> **Pergunta**
>
> Pra que 2 de cada?

- As funções `quot` e `rem` arredondam para o 0, enquanto `div` e `mod`
  para `-∞`.

### `Fractional` - classe de números fracionários

- A classe `Fractional` define todos os tipos numéricos fracionários e
  suas instâncias devem responder:

  ```hs
  (/) :: a -> a -> a
  recip :: a -> a
  ```

```console
> 10 / 3
> recip 10
```

### Outros operadores e funções úteis

Qual a diferença entre esses dois operadores de exponenciação?

```hs
(^) :: (Num a, Integral b) => a -> b -> a
(**) :: Floating a => a -> a -> a
```

### `Floating` - classe de números de ponto flutuante

```hs
class Fractional a => Floating a where
  pi :: a
  exp :: a -> a
  log :: a -> a
  sqrt :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a
```

### Info

- No `ghci`, o comando `:info` mostra informações sobre os tipos
  e as classes de tipo:

  ```console
  > :info Integral
  class (Real a, Enum a) => Integral a where
    quot :: a -> a -> a
    rem :: a -> a -> a
    div :: a -> a -> a
    mod :: a -> a -> a
    quotRem :: a -> a -> (a, a)
    divMod :: a -> a -> (a, a)
    toInteger :: a -> Integer
    {-# MINIMAL quotRem, toInteger #-}
  > :info Bool
  data Bool = False | True -- Defined in ‘GHC.Types’
  instance Eq Bool -- Defined in ‘GHC.Classes’
  instance Ord Bool -- Defined in ‘GHC.Classes’
  instance Show Bool -- Defined in ‘GHC.Show’
  instance Read Bool -- Defined in ‘GHC.Read’
  instance Enum Bool -- Defined in ‘GHC.Enum’
  instance Bounded Bool -- Defined in ‘GHC.Enum’
  ```

### Atividade

- Escreva as definições para os seguintes tipos em um arquivo
  `atividade02.hs` e carregue no `ghci`. Não importa o que ela
  faça, só não pode gerar erro:

  ```hs
  bools :: [Bool]
  nums :: [[Int]]
  soma :: Int -> Int -> Int -> Int
  copia :: a -> (a, a)
  f :: a -> a
  g :: Eq a => a -> (a, a) -> Bool
  h :: Num a => Int -> a -> a
  ```
