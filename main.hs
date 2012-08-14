import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import System.Random
import Data.Time.Clock

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
 
  stdGen <- getStdGen 
  let (randomValue, newGenerator) = randomR (0, 6) (stdGen) 

  fnt <- openFont "font.ttf" 30
  gameLoop (GameState True 0 (Block 4 0 0 randomValue) fnt 0 [] newGenerator) 

-- Main loop, one cycle per paint and logic tick
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

pieceR :: StdGen -> (Int, StdGen)
pieceR gen = randomR (0,6) gen

-- drop the active block once downwards
incrementBlock :: GameState -> GameState
incrementBlock gs =
 if (nextIsFree gs (0,1)) 
   then let b' = block gs
	    b = b' {y = ((y b')+1)}
        in gs {steps = 0, block = b}
   else let 
	    (val, gen') = pieceR (gen gs)
	    b = Block 0 0 0 val 
	    fld = permanentBlock (getBlockPositions gs) (field gs)
        in gs {block = b, field = fld, steps = 0, gen = gen' }

nextIsFree :: GameState -> (Int, Int)-> Bool
nextIsFree gs change = legalPosition (x (block gs) + (fst change)) (y (block gs) + (snd change)) gs 

makeBlks :: [(Int, Int)] -> Clr -> [Blk]
makeBlks [x] clr = [Blk x clr]
makeBlks (x:xs) clr = Blk x clr: makeBlks xs clr

permanentBlock :: [Blk] -> [Blk] -> [Blk]
permanentBlock a b = clearDone (merge a b)

clearDone ::[Blk] -> [Blk]
clearDone fld =
  let cnt = countElementPerLine fld
      rmLines = getRmLines 0 cnt
      fld' = clearLines fld rmLines
      fld'' = shiftLines fld' rmLines
      in fld''

shiftLines [] _ = []
shiftLines (Blk (a,b) c:xs) lst =
  let nrShifts = length $ filter (>b) lst
  in (Blk (a, b +nrShifts) c:shiftLines xs lst)

getRmLines :: Int -> [Int] -> [Int]
getRmLines _ [] = []
getRmLines curLine (x : xs) =
  case x >= width of
    True -> (curLine : getRmLines (curLine +1)  xs)
    otherwise -> getRmLines (curLine+1) xs

clearLines :: [Blk] -> [Int] -> [Blk]
clearLines [] _ = []
clearLines ( Blk (a,b) z : xs) rmLines 
  | elem b rmLines = clearLines xs rmLines
  | otherwise = (Blk (a,b) z:clearLines xs rmLines)

countElementPerLine :: [Blk] -> [Int]
countElementPerLine [] = take height (repeat 0)
countElementPerLine (Blk (a,b) _:s) =incrementElement b 1 (countElementPerLine s)

incrementElement :: Int -> Int -> [Int] -> [Int]
incrementElement a b [] = []
incrementElement a b (x:xs)
    | a == 0 = (x + b: xs)
    | otherwise =  (x : incrementElement (a-1) b xs)

merge :: [Blk] -> [Blk] -> [Blk]
merge [] other = other
merge (x:xs) other = x:merge xs other

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
    KeyDown (Keysym SDLK_UP _ _) -> rotate gs 
    KeyDown (Keysym SDLK_DOWN _ _) -> gs {steps = ticksPerStep}
    _ -> gs

handleEvents (x:xs) gs = handleEvents xs (handleEvents [x] gs)
handleEvents [] gs = gs

rotate :: GameState -> GameState
rotate gs = 
  let  rot' = ((rot (block gs))+ 1) `mod` (length (blocks !! (blockId (block gs))))  
       block' = (block gs) { rot = rot' } 
  in case nextIsFree gs{block = block'} (0,0) of
              False -> gs
              otherwise -> gs { block = block'} 

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
  
  paintField (getBlockPositions gs) s

  paintField (field gs) s 

  SDL.flip s

addPosition x y (Blk (a,b) clr) = Blk (a+x, b+y) clr

getBlockPositions gs = 
  let b = block gs
      curBlock = blocks !! (blockId b)
  in map (addPosition (x b) (y b)) (curBlock  !! (rot b))

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
-- orangeblock <- loadBMP' "data/orangeblock.bmp"

move :: Int -> Int -> GameState -> GameState
move x' y' gs = 
  let b' = block gs
      b = if(legalPosition (x'+ (x b')) (y' + (y b')) gs) then b' {x = (x b') + x', y = (y b') + y' } else b'
  in  gs {block = b}

legalPosition :: Int -> Int -> GameState -> Bool
legalPosition x y gs = 
  let b = block gs
      curBlock = blocks !! (blockId b)
      transPos = map (addPosition x y) (curBlock !! (rot b))
      res = not $ collission transPos (field gs)
  in res

collission :: [Blk] -> [Blk] -> Bool
collission a b = elem True (map (coll b) a)

coll :: [Blk] -> Blk -> Bool
coll [] (Blk (a,b) f)
  | b >= height -1 = True
  | a < 0 = True
  | a >= width = True
  | otherwise = False
coll (Blk (a,b) _ :xs) (Blk (d,e) f)
  | a == d && b == e = True
  | otherwise = coll xs (Blk (d,e) f)

