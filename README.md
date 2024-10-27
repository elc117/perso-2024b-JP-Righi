# perso-2024b-JP-Righi
perso-2024b-JP-Righi created by GitHub Classroom

# Jogo de Adivinhação de Cor

Este projeto consiste em um jogo simples onde o jogador tenta adivinhar uma cor alvo ajustando valores RGB. A cada tentativa, é mostrada a proximidade entre a cor ajustada e a cor alvo, que é gerada aleatoriamente em cada nova partida.

## Estrutura e Lógica do Código

O desenvolvimento foi feito com Haskell e a biblioteca Gloss, permitindo uma interface visual minimalista e interativa. Abaixo, explicamos a estrutura do código e o desenvolvimento de cada função principal, incluindo o raciocínio por trás das escolhas de design e as necessidades específicas que elas atendem.

### 1. Tipo `GameState` e Inicialização

Para organizar as informações sobre o estado do jogo, primeiro criamos um tipo de dados `GameState` com os seguintes campos:

- `targetColor`: Define a cor alvo em formato RGB `(Int, Int, Int)`, gerada aleatoriamente no início de cada jogo.
- `redValue`, `greenValue`, `blueValue`: Componentes RGB da cor ajustada pelo jogador.
- `guessMade`: Um `Bool` para verificar se o jogador finalizou uma tentativa de adivinhação.
- `proximity`: Indica o quão próximo o jogador está da cor alvo.
- `status`: Representa o estado do jogo (em andamento, finalizado ou tela de fim).
- `selection`: Define a seleção do jogador entre “Jogar Novamente” ou “Sair”.

### 2. Função `rgbColor`

Convertendo valores RGB para uma `Color` do Gloss:

```haskell
rgbColor :: Int -> Int -> Int -> Color
rgbColor r g b = makeColorI r g b 255
```

Essa função permite renderizar as cores no formato da biblioteca Gloss. Ela foi uma das primeiras funções implementadas, pois é essencial para a exibição de cores no jogo.

### 3. Função `rgbToHex` e `toHex`

Para maior clareza e um visual amigável, foi implementada uma conversão para o formato hexadecimal, comum em representações de cores. A função rgbToHex converte valores RGB para uma string hexadecimal:

```haskell
rgbToHex :: Int -> Int -> Int -> String
rgbToHex r g b = "#" ++ toHex r ++ toHex g ++ toHex b
```

A função toHex ajuda no cálculo:

```haskell
toHex :: Int -> String
toHex n = let hex = "0123456789ABCDEF"
          in [hex !! (n `div` 16), hex !! (n `mod` 16)]
```

### 4. Cálculo da Proximidade com calculateProximity

Uma função importante para a mecânica de feedback é a calculateProximity, que compara a cor ajustada com a cor alvo:

```haskell
calculateProximity :: (Int, Int, Int) -> (Int, Int, Int) -> Int
calculateProximity (r1, g1, b1) (r2, g2, b2) =
  let diff = abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2)
  in max 0 (100 - diff * 100 `div` 765)
```

O valor calculado é uma porcentagem, de 0 a 100, baseada na soma das diferenças entre os valores RGB, o que ajuda a dar um feedback sobre o quão próximo o jogador está.

### 5. Função renderGame

Para exibir o jogo, a função renderGame é responsável por mostrar a interface visual do jogo, com diferentes cenários de acordo com o estado (status):

- `Playing`: exibe o quadrado alvo e a cor que o jogador está ajustando.
- `GameOver`: exibe a cor alvo, a cor ajustada e a proximidade calculada.
- `EndScreen`: exibe as opções de “Jogar Novamente” ou “Sair”.

Essa função foi desenvolvida para garantir que o estado do jogo seja exibido corretamente para o jogador.


### 6. Manipulação de Inputs com handleInput

A função handleInput processa os comandos do jogador:

```haskell
handleInput :: Event -> GameState -> GameState
```

Os inputs permitem que o jogador ajuste valores RGB com teclas específicas e confirme com Enter. Quando Enter é pressionado no estado EndScreen, o jogo reinicia com uma nova cor gerada aleatoriamente.

### 7. Geração Aleatória da Cor Alvo

Essa parte foi uma das que deu bastante estresse. Muita dificuldade para gerar números aleatórios no haskell, não entendo o porque. Depois de muita internet, gpt, stackoverflow, consegui um resultado aceitável. Basicamente, pra evitar que a cor alvo seja a mesma a cada reinício, fiz a geração de valores aleatórios usando a função randomRIO, que permite definir targetColor de forma randômica em cada partida. O código foi adaptado para que isso aconteça sem reinicializar o jogo inteiro.

### 8. Função initialState

A função initialState agora usa a geração aleatória de targetColor para fornecer uma nova cor sempre que o jogo é reiniciado. Isso aprimora a experiência ao garantir que cada tentativa de jogo é única.