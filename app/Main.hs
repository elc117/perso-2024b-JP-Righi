import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- Data to hold the game state
data GameState = GameState
  { redValue   :: Int
  , greenValue :: Int
  , blueValue  :: Int
  , targetColor :: (Int, Int, Int)
  , guessMade  :: Bool
  , proximity  :: String
  }

-- Função para calcular a diferença entre duas cores
colorDiff :: (Int, Int, Int) -> (Int, Int, Int) -> Float
colorDiff (r1, g1, b1) (r2, g2, b2) = fromIntegral (abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2))

-- Função para determinar a proximidade
evaluateProximity :: Float -> String
evaluateProximity diff
  | diff < 30  = "Muito perto!"
  | diff < 100 = "Perto!"
  | diff < 200 = "Longe!"
  | otherwise  = "Muito longe!"

-- Function to convert RGB to Gloss Color
rgbColor :: Int -> Int -> Int -> Color
rgbColor r g b = makeColorI r g b 255

-- Função que transforma valores RGB em formato hexadecimal
rgbToHex :: Int -> Int -> Int -> String
rgbToHex r g b = "#" ++ toHex r ++ toHex g ++ toHex b

toHex :: Int -> String
toHex n = let hex = "0123456789ABCDEF"
          in [hex !! (n `div` 16), hex !! (n `mod` 16)]

-- Initial state of the game
initialState :: GameState
initialState = GameState 128 128 128 (255, 0, 0) False ""

-- Rendering function
renderGame :: GameState -> Picture
renderGame game =
  let (targetR, targetG, targetB) = targetColor game
  in pictures
       [ translate (-200) 0 $ color (rgbColor targetR targetG targetB) $ rectangleSolid 100 100
       , translate (-200) (-150) $ color white $ scale 0.2 0.2 $ text ("Cor alvo: " ++ rgbToHex targetR targetG targetB)
       , translate 100 0 $ color (rgbColor (redValue game) (greenValue game) (blueValue game)) $ rectangleSolid 100 100
       , translate 100 (-150) $ color white $ scale 0.2 0.2 $ text ("Sua cor: " ++ rgbToHex (redValue game) (greenValue game) (blueValue game))
       , if guessMade game
           then translate 100 (-250) $ color white $ scale 0.2 0.2 $ text ("Proximidade: " ++ proximity game)
           else blank
       ]



-- Function to update game state when key is pressed
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'r') Down _ _) game = game { redValue = min 255 (redValue game + 5) }
handleInput (EventKey (Char 'f') Down _ _) game = game { redValue = max 0 (redValue game - 5) }
handleInput (EventKey (Char 'g') Down _ _) game = game { greenValue = min 255 (greenValue game + 5) }
handleInput (EventKey (Char 'v') Down _ _) game = game { greenValue = max 0 (greenValue game - 5) }
handleInput (EventKey (Char 'b') Down _ _) game = game { blueValue = min 255 (blueValue game + 5) }
handleInput (EventKey (Char 'n') Down _ _) game = game { blueValue = max 0 (blueValue game - 5) }
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) game =
  let diff = colorDiff (redValue game, greenValue game, blueValue game) (targetColor game)
  in game { guessMade = True, proximity = evaluateProximity diff }
handleInput _ game = game

-- Main function
main :: IO ()
main = play
  (InWindow "Color Matching Game" (600, 400) (100, 100))
  black
  60
  initialState
  renderGame
  handleInput
  (const id) -- no need for updates based on time
