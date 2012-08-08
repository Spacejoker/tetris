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
  let fld = take 22 (repeat (take 10 (repeat 0))) :: [[Int]]
  gameLoop (GameState True 0 (Block 4 0 0) fnt 0 fld) 

gameLoop :: GameState -> IO ()
gameLoop gs = do
  events <- getEvents pollEvent []

  let gs' = handleEvents events gs

  delay 10 

  render gs'
 
  if (gameActive gs')
    then gameLoop  (tick gs')
    else return ()

tick :: GameState -> GameState
tick gs
  | steps gs > ticksPerStep = incrementBlock gs 
  | otherwise = gs {steps = (steps gs) + 1}

incrementBlock :: GameState -> GameState
incrementBlock gs = gs {steps = 0, block = b}
    where b' = block gs
	  b = b' {y = ((y b')+1)}

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
    KeyDown (Keysym SDLK_UP _ _) -> gs { block = (block gs) { rot = ((rot (block gs))+ 1) `mod` 2 } }
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

  title <- renderTextSolid (font gs) "Tetris" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 515 40 200 40))
  
  paintBlock (blockI !! (rot (block gs))) (x (block gs), y (block gs)) s

  paintField (field gs) s 0

  SDL.flip s

paintField :: [[Int]] -> Surface -> Int -> IO()
paintField [] _ _ = do return ()
paintField (x:xs) s height = do
  paintBlocks x s height 0
  paintField xs s (height+1)

paintBlocks :: [Int] -> Surface -> Int -> Int -> IO()
paintBlocks [] s y x = do return ()
paintBlocks (x:xs) s y' x' = do
  if x > 0 then paintBlock [(x', y')] (0, 0) s else return ()
  paintBlocks xs s y' (x'+1)

paintBlock :: [(Int, Int)] -> (Int, Int) -> Surface -> IO()
paintBlock [x] pos s = do 
  paintSquare x pos s 1
paintBlock (x:xs) pos s = do
  paintSquare x pos s 1
  paintBlock xs pos s

paintSquare :: (Int, Int) -> (Int, Int) -> Surface -> Int -> IO()
paintSquare pair pos s color = do
  let x' = ((fst pos) + fst pair) * dim + leftOffset
  let y' = ((snd pos) + snd pair) * dim + topOffset
  if color > 0 then fillRect s (Just (Rect x' y' dim dim)) (Pixel 1516) else return True 
  return ()
-- orangeblock <- loadBMP' "data/orangeblock.bmp"

move :: Int -> GameState -> GameState
move d gs = gs'
  where gs' = gs { block = b}
        b' = block $ gs
        b = if(legalPosition (x b' +d) (y b')) then b' {x = (x b') + d } else b'

legalPosition x y = x >= 0 && x <= 10 
