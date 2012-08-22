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

  stdGen <- getStdGen 
  let (randomValue, newGenerator) = randomR (0, 6) (stdGen) 
  let queuelength = 6
  let tmp = take queuelength (createRandomList newGenerator)
  let stdGen' = snd (tmp !! (queuelength -1))
  let queue' = map (\x -> fst x) tmp

  fnt <- openFont "font.ttf" 30
  redBlock <- loadBMP "images/red.bmp"
  blueBlock <- loadBMP "images/blue.bmp"
  orangeBlock <- loadBMP "images/orange.bmp"
  violetBlock <- loadBMP "images/violet.bmp"
  greenBlock <- loadBMP "images/green.bmp"
  yellowBlock <- loadBMP "images/yellow.bmp"
  cyanBlock <- loadBMP "images/cyan.bmp"

  bg <- loadBMP "bg.bmp"

  let newState = GameState True 0 (Block 4 0 0 randomValue) fnt 0 [] stdGen' Menu queue' bg [redBlock, blueBlock, orangeBlock, violetBlock, greenBlock, yellowBlock, cyanBlock]

  gameLoop newState
-- delegate = [(Menu, showMenu), (GamePlay, loopGame)]
-- Main loop, one cycle per paint and logic tick
gameLoop :: GameState -> IO ()
gameLoop gs = do
  --events <- getEvents pollEvent []

  --let gs' = handleIngameEvents events gs

  --delay 10 

  --render gs'
  --gs'' <- unwrap gs'
--  gs'
 -- let gs' = if mode gs == Menu then showMenu gs else loopGame gs
--  let b = 0

--  let gs'' = mapM gs'

  gs' <- tickGame gs-- if (mode gs) == GamePlay then showMenu gs else loopGame gs
 
  if (gameActive gs')
    then gameLoop  (tick gs')
    else return ()

tickGame :: GameState -> IO GameState
tickGame gs = do
  case (mode gs) of
    GamePlay -> loopGame gs
    otherwise -> showMenu gs

showMenu :: GameState -> IO GameState
showMenu gs = do
  putStrLn "in Menu"
  events <- getEvents pollEvent []
  let gs' = handleMenuEvents events gs

  delay 10

  renderMenu gs'
  return gs'

loopGame :: GameState -> IO GameState
loopGame gs = do
  events <- getEvents pollEvent []

  let gs' = handleIngameEvents events gs

  delay 10 

  render gs'
  return gs'

newGameState :: GameState -> GameState
newGameState gs = 
  let stdGen = gen gs 
      (randomValue, newGenerator) = randomR (0, 6) (stdGen) 
      queuelength = 6
      tmp = take queuelength (createRandomList newGenerator)
      stdGen' = snd (tmp !! (queuelength -1))
      queue' = map (\x -> fst x) tmp
  in gs {
	 score = 0, steps = 0, field = [], queue = queue', block = Block 4 0 0 randomValue, mode = GamePlay
}
--GameState True 0 (Block 4 0 0 randomValue) fnt 0 [] stdGen' Menu queue' bg [redBlock, blueBlock, orangeBlock, violetBlock, greenBlock, yellowBlock, cyanBlock]


-- stolen code from mr cadr, works nice
getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)

handleMenuEvents :: [Event] -> GameState -> GameState
handleMenuEvents [] gs = gs
handleMenuEvents (x:xs) gs = handleMenuEvents xs (handleMenuEvent x gs)

handleMenuEvent :: Event -> GameState -> GameState
handleMenuEvent x gs = 
  case x of
    KeyDown (Keysym SDLK_SPACE _ _) -> newGameState gs
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs { gameActive = False }
    _ -> gs

-- handle the different types of events
handleIngameEvent :: [Event] -> GameState ->  GameState
handleIngameEvent [x] gs =
  case x of 
    KeyDown (Keysym SDLK_RIGHT _ _) -> move 1 0 gs 
    KeyDown (Keysym SDLK_LEFT _ _) -> move (-1) 0 gs 
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs { gameActive = False }
    KeyDown (Keysym SDLK_UP _ _) -> rotate gs 
    KeyDown (Keysym SDLK_DOWN _ _) -> gs {steps = ticksPerStep}
    KeyDown (Keysym SDLK_SPACE _ _) -> gs {mode = Menu}
    Quit -> gs { gameActive = False}
    _ -> gs

handleIngameEvents (x:xs) gs = handleIngameEvents xs (handleIngameEvent [x] gs)
handleIngameEvents [] gs = gs

renderMenu :: GameState -> IO ()
renderMenu gs = do
  s <- getVideoSurface

  title <- renderTextSolid (font gs) "Menu Mega version olol" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 510 10 200 400))
 
  SDL.flip s

render :: GameState -> IO ()
render gs = do 

  s <- getVideoSurface

  blitSurface (bg gs) Nothing s (Just (Rect 0 0 800 600))

  title <- renderTextSolid (font gs) "Mega Haskell" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 510 10 200 400))

  title <- renderTextSolid (font gs) "Tetris" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 515 40 200 40))
  
  title <- renderTextSolid (font gs) ("Score: " ++ (show (score gs))) (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 515 80 200 40))
  
  paint' leftOffset topOffset (merge (getBlockPositions gs) (field gs)) s gs

  paintQueue (queue gs) 0 s gs

  SDL.flip s

paintQueue :: [Int] -> Int -> Surface -> GameState -> IO ()
paintQueue [] _ _ _ = return ()
paintQueue (x:xs) nr s gs= do 
  paint' 330 (nr*100) ((blocks !! x) !! (getShowRot x)) s gs
  paintQueue xs (nr+1) s gs

getShowRot :: Int -> Int
getShowRot x =
  case x of
    0 -> 1
    _ -> 0

paint' :: Int -> Int -> [Blk] -> Surface -> GameState -> IO Bool
paint' _ _ [] _ _ = return True 
paint' x y (blk:xs) s gs = do
  let x' = (fst (pos blk)) * dim + x
  let y' = (snd (pos blk)) * dim + y
  let bmp = getBrickGraphics gs blk
  blitSurface bmp Nothing s (Just (Rect x' y' dim dim)) 
  paint' x y xs s gs

getBrickGraphics :: GameState -> Blk -> Surface
getBrickGraphics gs blk = 
  case  color blk of 
     Red -> blockGraphics gs !! 0
     Blue -> blockGraphics gs !! 1
     Orange -> blockGraphics gs !! 2
     Violet -> blockGraphics gs !! 3
     Green -> blockGraphics gs !! 4
     Yellow -> blockGraphics gs !! 5
     _ -> blockGraphics gs !! 6
