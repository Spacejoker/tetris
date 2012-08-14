import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import System.Random

import Model
import Physics
import GameMechanics

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "Mega Haskell Tetris" "efefef" 

  enableKeyRepeat 500 30
 
  bg <- loadBMP "bg.bmp"

  stdGen <- getStdGen 
  let (randomValue, newGenerator) = randomR (0, 6) (stdGen) 
  let queue' = take 10 (createRandomList newGenerator)
  fnt <- openFont "font.ttf" 30
  gameLoop (GameState True 0 (Block 4 0 0 randomValue) fnt 0 [] newGenerator GamePlay queue' bg)

-- Main loop, one cycle per paint and logic tick
gameLoop :: GameState -> IO ()
gameLoop gs = do
  events <- getEvents pollEvent []

  let gs' = handleIngameEvents events gs

  delay 10 

  render gs'
 
  if (gameActive gs')
    then gameLoop  (tick gs')
    else return ()

-- stolen code from mr cadr, works nice
getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)

-- handle the different types of events
handleIngameEvent :: [Event] -> GameState ->  GameState
handleIngameEvent [x] gs =
  case x of 
    KeyDown (Keysym SDLK_RIGHT _ _) -> move 1 0 gs 
    KeyDown (Keysym SDLK_LEFT _ _) -> move (-1) 0 gs 
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs { gameActive = False }
    KeyDown (Keysym SDLK_UP _ _) -> rotate gs 
    KeyDown (Keysym SDLK_DOWN _ _) -> gs {steps = ticksPerStep}
    _ -> gs

handleIngameEvents (x:xs) gs = handleIngameEvents xs (handleIngameEvent [x] gs)
handleIngameEvents [] gs = gs

render :: GameState -> IO ()
render gs = do 

  s <- getVideoSurface
  -- Clear the screen
  worked <- fillRect s Nothing  (Pixel 0)

  blitSurface (bg gs) Nothing s (Just (Rect 0 0 800 600))

  title <- renderTextSolid (font gs) "Mega Haskell" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 510 10 200 400))

  title <- renderTextSolid (font gs) "Tetris" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 515 40 200 40))
  
  title <- renderTextSolid (font gs) ("Score: " ++ (show (score gs))) (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 515 80 200 40))
  
  paintField (getBlockPositions gs) s

  paintField (field gs) s 

  paintQueue (queue gs) 0 s

  SDL.flip s

paintField :: [Blk] -> Surface -> IO()
paintField [] _  = do return ()
paintField (x:xs) s = do
  paintSquare x s 
  paintField xs s 

paintSquare :: Blk -> Surface -> IO Bool
paintSquare blk s = do
  let x' = (fst (pos blk)) * dim + leftOffset
  let y' = (snd (pos blk)) * dim + topOffset
  fillRect s (Just (Rect x' y' dim dim)) (Pixel 255) 

paintQueue :: [Int] -> Int -> Surface -> IO ()
paintQueue [] _ _ = return ()
paintQueue (x:xs) nr s = do 
  paint' 330 (nr*100) ((blocks !! x) !! (getShowRot x)) s 
  paintQueue xs (nr+1) s

getShowRot :: Int -> Int
getShowRot x =
  case x of
    0 -> 1
    _ -> 0

paint' :: Int -> Int -> [Blk] -> Surface -> IO Bool
paint' _ _ [] _= return True 
paint' x y (blk:xs) s = do
  let x' = (fst (pos blk)) * dim + x
  let y' = (snd (pos blk)) * dim + y
--  putStrLn (show x') ++ " " ++ (show y')
  fillRect s (Just (Rect x' y' dim dim)) (Pixel 255) 
  paint' x y xs s

