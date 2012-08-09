import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Model

dim = 24
width = 10
height = 22
leftOffset = 20
topOffset = 20
ticksPerStep = 50

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "Mega Haskell Tetris" "efefef" 

  enableKeyRepeat 500 30

  fnt <- openFont "font.ttf" 30
  let fld = [(a, b, 0) | a <- [0..12], b <- [0..21]]
  gameLoop (GameState True 0 (Block 4 0 0 2) fnt 0 fld) 

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
incrementBlock gs =
 if (nextIsFree gs) 
   then let b' = block gs
	    b = b' {y = ((y b')+1)}
        in gs {steps = 0, block = b}
   else let b = Block 0 0 0 0
	    fld = permanentBlock (getBlockPositions gs) (field gs)
        in gs {block = b, field = fld, steps = 0 }

permanentBlock :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
permanentBlock a b = map (freezeBlock a) b

freezeBlock :: [(Int, Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
freezeBlock [] b = b
freezeBlock ((a,b,c):xs) (d,e,f)
  | a== d && b == e && c /= 0 = (a,b,c)
  | otherwise = freezeBlock xs (d,e,f)--(d, e, f)

nextIsFree :: GameState -> Bool
nextIsFree gs = legalPosition (x (block gs)) (y (block gs) + 1) gs 

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
    KeyDown (Keysym SDLK_RIGHT _ _) -> move 1 0 gs 
    KeyDown (Keysym SDLK_LEFT _ _) -> move (-1) 0 gs 
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs { gameActive = False }
    KeyDown (Keysym SDLK_UP _ _) -> gs { block = (block gs) { rot = ((rot (block gs))+ 1) `mod` (length (blocks !! (blockId (block gs)))) } }
    KeyDown (Keysym SDLK_DOWN _ _) -> gs {steps = ticksPerStep}
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
  
  paintBlock (getBlockPositions gs) s

  paintField (field gs) s 0

  SDL.flip s

addPosition x y (a, b, c) = (a+x, b+y, c)

getBlockPositions gs = 
  let b = block gs
      curBlock = blocks !! (blockId b)
  in map (addPosition (x b) (y b)) (curBlock  !! (rot b))

paintField :: [(Int, Int, Int)] -> Surface -> Int -> IO()
paintField [] _ _ = do return ()
paintField ((a, b, c):xs) s height = do
  paintBlock [(a, b, c)] s 
  paintField xs s (height+1)

paintBlock :: [(Int, Int, Int)] -> Surface -> IO()
paintBlock [x] s = do 
  paintSquare x s
paintBlock (x:xs) s= do
  paintSquare x s
  paintBlock xs s

paintSquare :: (Int, Int, Int) -> Surface -> IO()
paintSquare (a, b, c) s = do
  let x' = a * dim + leftOffset
  let y' = b * dim + topOffset
  if c > 0 then fillRect s (Just (Rect x' y' dim dim)) (Pixel 255) else return True 
  return ()
-- orangeblock <- loadBMP' "data/orangeblock.bmp"

move :: Int -> Int -> GameState -> GameState
move x' y' gs = 
  let b' = block gs
      b = if(legalPosition (x'+ (x b')) (y' + (y b')) gs) then b' {x = (x b') + x', y = (y b') + y' } else b'
  in  gs {block = b}
--  where gs' = gs { block = b }
--       b' = block gs
--       b = if(legalPosition x' y' gs) then b' {x = (x b') + x, y = (y b') + y' } else b'

legalPosition :: Int -> Int -> GameState -> Bool
legalPosition x y gs = 
  let b = block gs
      curBlock = blocks !! (blockId b)
      transPos = map (addPosition x y) (curBlock !! (rot b))
      res = not $ collission transPos (field gs)
  in res

collission :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
collission a b = elem True (map (coll a) b)

coll :: [(Int, Int, Int)] -> (Int, Int, Int) -> Bool
coll [] b = False
coll ((a,b,c):xs) (d,e,f)
  | c /= 0 && a < 0 = True
  | c /= 0 && a > width = True
  | a == d && b == e && c /= 0 && f /= 0 = True
  | b >= height - 1 && c /= 0 = True
  | otherwise = coll xs (d,e,f)

