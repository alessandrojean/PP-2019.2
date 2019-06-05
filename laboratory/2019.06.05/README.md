# Introdução
*Retirado do roteiro de aula do Prof. Emilio Francesquini, CMCC/UFABC.*

## Haskell

- Surgiu em 1990 com o objetivo de ser a primeira linguagem 
  puramente funcional.
- Por muito tempo considerada uma linguagem acadêmica.
- Atualmente é utilizada em diversas empresas (totalmente 
  ou em parte de projetos).

### Características

Por ter sido criada por um comitê de estudiosos de linguagem
de programação funcional e com a mentalidade de mantê-la
útil para o ensino e pesquisa de linguagem de programação,
assim como uso em empresas, a linguagem adquiriu diversas
características distintas e interessantes não observadas em
outras linguagens.

- **Códigos concisos e declarativos:** o programador *declara* o que ele
  quer ao invés de escrever um passo-a-passo. Programas em Haskell
  chegam a ser dezenas de vezes menores que em outras linguagens.

  ```haskell
  take 100 [x | x <- N, primo x]
  ```

- **Sistema de tipagem forte:** ao contrário de linguagens como *Java*
  e *C*, as declarações de tipo no Haskell são simplificadas (e muitas
  vezes podem ser ignoradas), porém, seu sistema rigoroso permite
  que muitos erros comuns sejam detectados em tempo de **compilação**.

  ```java
  int x = 10;
  double y = 5.1;
  System.out.println("Resultado: " + (x * y));
  ```

  ```hs
  x = 10 :: Int
  y = 5.1 :: Double
  print ("Resultado: " + (x * y))
  ```

- **Compreensão de listas:** listas são frequentemente utilizadas para
  a solução de diversos problemas. O Haskell utiliza listas como um de
  seus conceitos básicos permitindo uma notação muito parecida com a
  notação de conjuntos na matemática.

  ```
  xs = {x | x ∈ N, x impar}
  ```

  ```hs
  xs = [x | x <- N, impar x]
  ```

- **Imutabilidade:** não existe um conceito de variável, apenas nomes
  e declarações. Uma vez que um nome é declarado com um valor, ele não
  pode sofrer alterações.

  ```hs
  x = 1.0
  x = 2.0
  ```

- **Funções Recursivas:** com a imutabilidade, o conceito de laços de
  repetição também não existe em linguagens funcionais. Eles são implementados
  através de funções recursivas.

  ```c
  int x = 1;
  for (int i = 1; i <= 10; i++) {
    x = x * 2;
  }
  printf("%d\n", x);
  ```

  ```hs
  f 0 = 1
  -- Note que f(x) é o mesmo que f x
  f n = 2 * f (n - 1)
  print (f 10)
  ```

- **Funções de alta ordem:** funções podem receber funções como parâmetros.
  Isso permite definir funções genéricas, compor duas ou mais funções
  e definir linguagens de domínio específicos (ex.: *parsing*).

  ```hs
  print (aplique dobro [1, 2, 3, 4])
  > [2,4,6,8]

- **Tipos polimórficos:** permite definir funções genéricas que funcionam
  para classes de tipos. Por exemplo, o operador de soma `+` pode ser
  utilizado para qualquer tipo numérico.

  ```hs
  1 + 2         -- 3
  1.0 + 3.0     -- 4.0
  (2%3) + (3%6) -- (7%6)
  ```

- **Avaliação preguiçosa:** ao aplicar uma função, o resultado será
  computado apenas quando requisitado. Isso permite evitar computações
  desnecessárias, estimula uma programação modular e permite estruturas
  de dados infinitos.

  ```hs
  listaInf = [1..] -- 1, 2, 3, ...
  print (take 10 listaInf)
  ```

- **Raciocínio equacional:** podemos usar expressões algébricas para
  otimizar nosso programa ou provar sua corretude.

## Ambiente de Programação

### GHC e GHCI

- **GHCi Haskell Compiller (GHC):** compilador de código aberto para
  a linguagem Haskell.
  - Padrão de fato.
  - Outros compiladores existem mas são incompletos ou têm uma equipe
    limitada de manutenção.
- Possui um modo interativo **ghci** (similar ao **iPython**).
  - REPL - Read, Evaluate, Print, Loop.

### Glasgow Haskell Compiler

Uso recomendado de:
- **Git** - controle de revisão.
- **Stack** - gerenciamento de projeto e dependências.
- **Haddock** - documentação.

### Instalação - Opção 1 - Haskell Platform

- https://www.haskell.org/downloads#platform
- Para o Linux escolha a distribuição *Generic*, mesmo que tenha pacote
  para sua distribuição.
- Vários sabores de SOs disponíveis.

### Instalação - Opção 2 - Stack

- **Atenção!** Não utilize o `apt-get` para instalar o GHC ou o Stack.
- Para instalar o Stack no Linux:

  ```bash
  curl -sSL https://get.haskellstack.org/ | sh
  ```
- Para instalar no Windows faça o download do instalador no site
  https://docs.haskellstack.org/

### Verificando a instalação

```console
$ stack new primeiroProjeto simple
$ cd primeiroProjeto
$ stack setup
$ stack build
$ stack exec primeiroProjeto
```

### Editores recomendados

- Diversos editores de texto tem suporte à edição, compilação e execução
  de código Haskell. Entre eles estão Emacs, Vim, Atom, Sublime e Visual
  Studio Code. Todos baseados no Intero, um backend para IDEs de Haskell.
- Fique a vontade para escolher o editor da sua preferência. Em seguida,
  descreveremos as instruções para utilizar o Intero com o Atom e com o
  MS Visual Studio Code.

#### Atom

- Atom - https://atom.io/
  - Com os pacotes:
    - `haskell-grammar`
    - `language-haskell`

#### VSCode

Acesse https://code.visualstudio.com/ e baixe a versão compatível com o seu SO.

Após o download, nas máquinas com Ubuntu:

```console
$ sudo dpkg -i nome_do_arquivo.deb
```

Em seguida, precisamos instalar o Intero, hlint.

```console
$ stack install intero hlint apply-refact
```

Com a instalação concluída, abra o Visual Studio Code, no canto inferior
esquerdo clique na engrenagem, Extensions e instale a extensão **Haskero**
e **haskell-linter**.

### Interpretador GHCi

Se você tiver instalado o GGC usando Stack, substitua `ghci` abaixo
por `stack ghci`.

```console
$ ghci
> 2 + 3 * 4
14

> (2 + 3) * 4
20

> sqrt (3 ^ 2 + 4 ^ 2)
5.0
```

A função de exponenciação (`^`) tem prioridade maior do que multiplicação
e divisão (`*`, `/`) que por sua vez tem prioridade maior que a soma e
subtração (`+`, `-`).

```console
$ ghci
> 2 + 3 * 4 ^ 5 == 2 + (3 * (4 ^ 5))
True
```

### Informação de operadores

Para saber a prioridade de um operador basta digitar:

```console
> :i (+)
class Num a where
  (+) :: a -> a -> a
  ...
    -- Defined in 'GHC.Num'
infixl 6 +
```

A informação indica que `+` é um operador que pode ser utilizado para
qualquer tipo numérico, tem precedência nível 6 (quanto maior o número
maior sua prioridade) e é associativo a esquerda. Ou seja: `1 + 2 + 3`
vai ser computado na ordem `(1 + 2) + 3`.

### Funções

- Na matemática a aplicação de funções em seus argumentos é definida
  pelo nome da função e os parâmetros entre parênteses.
- A expressão `f(a, b) + c * d` representa a aplicação de `f` nos
  parâmetros `a` e `b` e, em seguida, a soma do resultado com o resultado
  do produto entre `c` e `d`.
- Em Haskell, a aplicação de fundo é definida como o nome da função
  seguido dos parâmetros separados por espaço com a maior prioridade
  na aplicação da função. O exemplo anterior ficaria:

  ```hs
  f a b + c * d
  ```

A tabela abaixo contém alguns contrastes entre a notação 
matemática e o Haskell:

| Matemática   | Haskell     |
| ------------ | ----------- |
| `f(x)`       | `f x`       |
| `f(x, y)`    | `f x y`     |
| `f(g(x))`    | `f (g x)`   |
| `f(x, g(y))` | `f x (g y)` |
| `f(x)g*y)`   | `f x * g y` |

### Hello World

Criem um arquivo `teste.hs`, abram no editor e no mesmo diretório iniciem
o GHCi. No arquivo digitem:

```hs
dobra x = x + x

quadruplica x = dobra (dobra x)
```

No GHCi:

```console
> :l teste.hs
> quadruplica 10
40
```

O comando `:l` carrega as definições contidas em um arquivo fonte.

Acrescentem a seguinte linha no arquivo fonte:

```hs
fatorial n = product [1..n]
```

e no GHCi:

```console
> :reload
> fatorial 5
120
```

### Outros comandos do GHCi

O comando `:t` mostra o tipo da função enquanto o comando `:q` sai do ghci.

```console
> :t dobra
dobra :: Num a => a -> a

> :q
$
```

### Outras coisas interessantes a saber

- `:h` - Imprime a ajuda.
- `:{` seguido de comandos e finalizado por `:}` permite comandos
  em múltiplas linhas.
  - Também é possível separar as linhas com `;`.

```console
> :{
| fatorial 0 = 1
| fatorial n = n * fatorial (n - 1)
| :}
> fatorial 5
120
> fatorial 2 0 = 1 ; fatorial2 n = n * fatorial2 (n - 1)
> fatorial2 7
5040
```

## Convenções

### Requisitos dos nomes

- Os nomes das funções e seus argumentos devem começar com uma letra
  minúscula e seguida por zero ou mais letras, maiúsculas ou minúsculas,
  dígitos, *underscore*, e aspas simples:

  ```
  funcao, ordenaLista, soma1, x'
  ```

### Nomes reservados

- Os únicos nomes que não podem ser utilizados são:

  ```
  case, class, data, default, deriving do, else,
  foreign, if, import, in, infix, infixl, infixr,
  instance, let module, newtype, of, then, type,
  where
  ```

### Convenção para listas

- As listas são nomeadas acrescentando o caractere `s` ao nome do que
  ela representa.
- Uma lista de números `n` é nomeada `ns`, uma lista de variáveis
  `x` se torna `xs`. Uma lista de listas de caracteres tem o nome `css`.

### Regra de layout

- O layout dos códigos em Haskell é similar ao de Python, em que os
  blocos lógicos são definidos pela indentação.

  ```hs
  f x = a * x + b
      where
        a = 1
        b = 3
  z = f 2 + 3
  ```

- A palavra-chave `where` faz parte da definição de `f`, da mesma forma,
  as definições de `a` e `b` fazem parte da cláusula `where`. A definição
  de `z` não faz parte de `f`.

### Tabs vs Espaço

- A definição de tabulação varia de editor para editor.
- Ainda que seja o mesmo editor, a tabulação varia de
  usuário para usuário.
- Como o espaço é importante no Haskell, usem espaços
  em vez de tab.

### Comentários

Comentários em uma linha são demarcados pela sequência `--`, comentários
em múltiplas linhas são demarcados por `{-` e `-}`.

```hs
-- Função que dobra o valor de x.
dobra x = x + x

{-
  Dobra recebe uma variável numérica
  e retorna seu valor em dobro.
-}
```

## Primeiro Projeto

- Para criar projetos, utilizaremos a ferramenta `stack`. Essa ferramenta
  cria um ambiente isolado.

```console
$ stack new primeiro-projeto simple
$ cd primeiro-projeto
$ stack setup
$ stack build
$ stack exec primeiro-projeto
```

Os dois últimos comandos são referentes a compilação do projeto e execução.

### Stack

O Stack cria a seguinte estrutura de diretório:

- `LICENSE`: informação sobre a licença de uso do software.
- `README.md`: informações sobre o projeto em formato Markdown.
- `Setup.hs`: retrocompatibilidade com o sistema cabal.
- `primeiro-projeto.cabal`: informações das dependências do projeto.
  Atualizado automaticamente pelo Stack.
- `stack.yaml`: parâmetros do projeto.
- `package.yaml`: configurações de compilação e dependências de 
  bibliotecas externas.
- `src/Main.hs`: arquivo principal do projeto.

### `Main.hs`

```hs
-- Indica que é o módulo principal.
module Main where

main :: IO ()
-- Início da função principal.
main = do
  -- Imprime hello world.
  putStrLn "hello world"
```

### Atividade

- Modifique o código `Main.hs` do `primeiro-projeto` criando uma
  função `triplo` que multiplica um valor `x` por `3`.

Modifique a função `main` da seguinte forma para testar:

```hs
main :: IO ()
main = do
  print (triplo 2)
```

### Atividade 2

- Crie um programa que utilizando 3 valores (provas, atividades, projeto)
  calcule a média (numérica) da disciplina.
