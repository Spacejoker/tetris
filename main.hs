import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Model

dim = 18
width = 10
heght = 22
leftOffset = 20
topOffset = 20
ticksPerStep = 50

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "Tetris deluxe" "efefef" 

  enableKeyRepeat 500 30

  fnt <- openFont "font.ttf" 30

  gameLoop (GameState True 0 (Block 4 0 0) fnt 0) 

gameLoop :: GameState -> IO ()
gameLoop gs = do
  events <- getEvents pollEvent []

  let gs' = handleEvents events gs

  delay 10 

  render gs'
 
  if (gameActive gs')
    then gameLoop  (tick gs')
    else quit'

tick :: GameState -> GameState
tick gs
  | steps gs > ticksPerStep = incrementBlock gs 
  | otherwise = gs {steps = (steps gs) + 1}

incrementBlock :: GameState -> GameState
incrementBlock gs = gs {steps = 0, block = b}
    where b' = block gs
	  b = b' {y = ((y b')+1)}

quit' :: IO ()
quit' = return ()

-- stolen code from mr cadr, works nice
getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)

-- handle the different types of events
handleEvents :: [Event] -> GameState ->  GameState
handleEvents [x] gs =
  case x of 
    KeyDown (Keysym SDLK_RIGHT _ _) -> move 1 gs 
    KeyDown (Keysym SDLK_LEFT _ _) -> move (-1) gs 
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs { gameActive = False }
    KeyDown (Keysym SDLK_SPACE _ _) -> gs { block = (block gs) { rot = ((rot (block gs))+ 1) `mod` 2 } }
    _ -> gs

handleEvents (x:xs) gs = handleEvents xs (handleEvents [x] gs)
handleEvents [] gs = gs

render :: GameState -> IO ()
render gs = do 

  s <- getVideoSurface
  -- Clear the screen
  worked <- fillRect s
            Nothing
            (Pixel 0)

  title <- renderTextSolid (font gs) "Mega Haskell" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 510 10 200 400))

  title <- renderTextSolid (font gs) "Tetris Clone" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 515 40 200 40))
  
  paintBlock (blockI !! (rot (block gs)))(block gs) s

  SDL.flip s

paintBlock :: [(Int, Int)] -> Block -> Surface -> IO()
paintBlock [x] block s = do 
  paintSquare x block s
paintBlock (x:xs) block s = do
  paintSquare x block s 
  paintBlock xs block s

paintSquare :: (Int, Int) -> Block -> Surface -> IO()
paintSquare pair block s = do
  let x' = ((x block) + fst pair) * dim + leftOffset
  let y' = ((y block) + snd pair) * dim + topOffset
  fillRect s (Just (Rect x' y' dim dim)) (Pixel 1516)
  return ()
-- orangeblock <- loadBMP' "data/orangeblock.bmp"

move :: Int -> GameState -> GameState
move d gs = gs'
  where gs' = gs { block = b}
        b' = block $ gs
        b = if(legalPosition (x b' +d) (y b')) then b' {x = (x b') + d } else b'

legalPosition x y = x >= 0 && x <= 10 
