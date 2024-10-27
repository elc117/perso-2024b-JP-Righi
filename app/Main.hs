import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

-- Define um tipo de estado do jogo para diferenciar entre o jogo em andamento, a tela de fim e a tela de escolha final
data GameStatus = Playing | GameOver | EndScreen deriving (Eq)

data GameState = GameState
  { targetColor :: (Int, Int, Int)
  , redValue :: Int
  , greenValue :: Int
  , blueValue :: Int
  , guessMade :: Bool
  , proximity :: Int
  , status :: GameStatus
  , selection :: Int  -- 0 para "Jogar Novamente", 1 para "Sair"
  , restart :: Bool   -- Indica se o jogo deve reiniciar
  }

-- Função para converter RGB para Gloss Color
rgbColor :: Int -> Int -> Int -> Color
rgbColor r g b = makeColorI r g b 255

-- Função que transforma valores RGB em formato hexadecimal
rgbToHex :: Int -> Int -> Int -> String
rgbToHex r g b = "#" ++ toHex r ++ toHex g ++ toHex b

toHex :: Int -> String
toHex n = let hex = "0123456789ABCDEF"
          in [hex !! (n `div` 16), hex !! (n `mod` 16)]

-- Função para calcular a proximidade entre duas cores
calculateProximity :: (Int, Int, Int) -> (Int, Int, Int) -> Int
calculateProximity (r1, g1, b1) (r2, g2, b2) =
  let diff = abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2)
  in max 0 (100 - diff * 100 `div` 765) -- valor entre 0 e 100

-- Função para gerar uma nova cor aleatória
generateRandomColor :: IO (Int, Int, Int)
generateRandomColor = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)
    return (r, g, b)

-- Função de renderização do jogo
renderGame :: GameState -> Picture
renderGame game =
  let (targetR, targetG, targetB) = targetColor game
      (currentR, currentG, currentB) = (redValue game, greenValue game, blueValue game)
      targetHex = rgbToHex targetR targetG targetB
      currentHex = rgbToHex currentR currentG currentB
      targetProximity = calculateProximity (targetR, targetG, targetB) (currentR, currentG, currentB)
  in pictures $
     case status game of
       Playing ->
         [ -- Quadrado alvo sem exibir o código RGB
           translate (-200) 0 $ color (rgbColor targetR targetG targetB) $ rectangleSolid 100 100
         , -- Código hexadecimal sendo editado pelo jogador
           translate 100 (-150) $ color white $ scale 0.2 0.2 $ text ("Sua cor: " ++ currentHex)
         ]
       GameOver ->
         [ -- Quadrado alvo
           translate (-200) 0 $ color (rgbColor targetR targetG targetB) $ rectangleSolid 100 100
         , -- Exibe o quadrado editado com a cor final definida
           translate 100 0 $ color (rgbColor currentR currentG currentB) $ rectangleSolid 100 100
         , -- Código hexadecimal que foi editado pelo jogador
           translate 100 (-150) $ color white $ scale 0.2 0.2 $ text ("Sua cor: " ++ currentHex)
         , -- Exibe o código RGB do alvo e a proximidade após o jogo terminar
           translate (-200) (-150) $ color white $ scale 0.2 0.2 $ text ("Cor alvo: " ++ targetHex)
         , translate 100 (-250) $ color white $ scale 0.2 0.2 $ text ("Proximidade: " ++ show targetProximity ++ "%")
         ]
       EndScreen ->
         [ translate (-370) 50 $ color white $ scale 0.3 0.3 $ text "Pressione Enter para jogar novamente"
         , translate (-270) (-50) $ color white $ scale 0.3 0.3 $ text "Ou pressione Esc para sair"
        --  , translate (-100) (if selection game == 0 then 50 else (-50)) $
        --    color white $ scale 0.3 0.3 $ text "->"  -- seta de seleção
         ]

-- Função para lidar com o clique de Enter e finalizar o jogo
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'r') Down _ _) game
  | status game == Playing = game { redValue = min 255 (redValue game + 5) }
handleInput (EventKey (Char 'f') Down _ _) game
  | status game == Playing = game { redValue = max 0 (redValue game - 5) }
handleInput (EventKey (Char 'g') Down _ _) game
  | status game == Playing = game { greenValue = min 255 (greenValue game + 5) }
handleInput (EventKey (Char 'v') Down _ _) game
  | status game == Playing = game { greenValue = max 0 (greenValue game - 5) }
handleInput (EventKey (Char 'b') Down _ _) game
  | status game == Playing = game { blueValue = min 255 (blueValue game + 5) }
handleInput (EventKey (Char 'n') Down _ _) game
  | status game == Playing = game { blueValue = max 0 (blueValue game - 5) }
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) game
  | status game == Playing = game { guessMade = True, proximity = calculateProximity (targetColor game) (redValue game, greenValue game, blueValue game), status = GameOver }
  | status game == GameOver = game { status = EndScreen }
  | status game == EndScreen = 
      unsafePerformIO $ do
          newColor <- generateRandomColor
          return game { targetColor = newColor, status = Playing, guessMade = False }  -- indicar que o jogo deve ser reiniciado
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game
  | status game == EndScreen = game { selection = 0 }  -- selecionar "Jogar Novamente"
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game
  | status game == EndScreen = game { selection = 1 }  -- selecionar "Sair"
handleInput _ game = game

-- Inicialização do estado do jogo com valores aleatórios
initialState :: IO GameState
initialState = do
  targetR <- randomRIO (0, 255)
  targetG <- randomRIO (0, 255)
  targetB <- randomRIO (0, 255)
  return GameState
    { targetColor = (targetR, targetG, targetB)
    , redValue = 127
    , greenValue = 127
    , blueValue = 127
    , guessMade = False
    , proximity = 0
    , status = Playing
    , selection = 0
    , restart = False
    }

-- Converte `initialState` com `unsafePerformIO` para reinicializar o jogo
resetGameState :: GameState
resetGameState = unsafePerformIO initialState

-- Função main do jogo
main :: IO ()
main = play window backgroundColor fps resetGameState renderGame handleInput updateGame
  where
    window = InWindow "Jogo de Adivinhar a Cor" (800, 600) (100, 100)
    backgroundColor = black
    fps = 60

-- Função de atualização que reinicia o jogo se necessário
updateGame :: Float -> GameState -> GameState
updateGame _ game
  | restart game = resetGameState  -- Reinicializa o estado se restart for True
  | otherwise = game