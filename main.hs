import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import System.Random

import Model
import Physics
import GameMechanics
import Render

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "Jens Mega Haskell Tetris" "Jens Tetris" 

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
  menubg <- loadBMP "menubg.bmp"

  newmenu0 <- SDLi.load "images/newgame0.png"
  newmenu1 <- SDLi.load "images/newgame1.png"
  newmenu2 <- SDLi.load "images/newgame2.png"
  newmenu3 <- SDLi.load "images/newgame3.png"
  newmenu4 <- SDLi.load "images/newgame4.png"
  let newgameAnim = [newmenu0, newmenu1, newmenu2, newmenu3, newmenu4, newmenu3, newmenu2, newmenu1]

  credits0 <- SDLi.load "images/credits0.png"
  credits1 <- SDLi.load "images/credits1.png"
  credits2 <- SDLi.load "images/credits2.png"
  credits3 <- SDLi.load "images/credits3.png"
  credits4 <- SDLi.load "images/credits4.png"
  let creditAnimation = [credits0, credits1,  credits2,  credits3,  credits4,  credits3,  credits2,  credits1] 
  
  q0 <- SDLi.load "images/quit0.png"
  q1 <- SDLi.load "images/quit1.png"
  q2 <- SDLi.load "images/quit2.png"
  q3 <- SDLi.load "images/quit3.png"
  q4 <- SDLi.load "images/quit4.png"
  let quitAnim = [q0, q1, q2, q3, q4, q3, q2, q1]
 
  cm <- SDLi.load "images/choicemarker.png"
 
  let graphics = Graphics bg [redBlock, blueBlock, orangeBlock, violetBlock, greenBlock, yellowBlock, cyanBlock] menubg newgameAnim creditAnimation quitAnim cm
  let newState = GameState True 0 (Block 4 0 0 randomValue) fnt 0 [] stdGen' Menu queue' graphics False 0

  gameLoop newState

gameLoop :: GameState -> IO ()
gameLoop gs = do
  gs' <- tickGame gs
 
  if (gameActive gs')
    then gameLoop gs'
    else return ()

tickGame :: GameState -> IO GameState
tickGame gs = do
  case (mode gs) of
    GamePlay -> loopGame gs
    otherwise -> showMenu gs

showMenu :: GameState -> IO GameState
showMenu gs = do
  events <- getEvents pollEvent []
  let gs' = handleMenuEvents events gs

  delay 10

  renderMenu gs'
  return gs' { steps = (steps gs) + 1}

loopGame :: GameState -> IO GameState
loopGame gs = do
  events <- getEvents pollEvent []

  let gs' = handleIngameEvents events gs

  delay 10 

  render gs'

  case lost gs of
    False -> return (tick gs')
    True -> return gs'

newGameState :: GameState -> GameState
newGameState gs = 
  let stdGen = gen gs 
      (randomValue, newGenerator) = randomR (0, 6) (stdGen) 
      queuelength = 6
      tmp = take queuelength (createRandomList newGenerator)
      stdGen' = snd (tmp !! (queuelength -1))
      queue' = map (\x -> fst x) tmp
  in gs { score = 0, steps = 0, field = [], queue = queue', block = Block 4 0 0 randomValue, mode = GamePlay, lost = False }

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
    KeyDown (Keysym SDLK_RETURN _ _) -> case menuchoice gs of 
					  0 -> newGameState gs
					  _ -> gs {gameActive = False}
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs { gameActive = False }
    KeyDown (Keysym SDLK_DOWN _ _) -> gs {menuchoice  = min ((menuchoice gs) + 1) 2}
    KeyDown (Keysym SDLK_UP _ _) -> gs {menuchoice  = max ((menuchoice gs) - 1) 0}
    _ -> gs

-- handle the different types of events
handleIngameEvent :: [Event] -> GameState ->  GameState
handleIngameEvent [x] gs =
  case x of 
    KeyDown (Keysym SDLK_RIGHT _ _) -> move 1 0 gs 
    KeyDown (Keysym SDLK_LEFT _ _) -> move (-1) 0 gs 
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs { mode = Menu }
    KeyDown (Keysym SDLK_RETURN _ _) -> gs { mode = Menu }
    KeyDown (Keysym SDLK_UP _ _) -> rotate gs 
    KeyDown (Keysym SDLK_DOWN _ _) -> gs {steps = ticksPerStep}
    KeyDown (Keysym SDLK_SPACE _ _) -> gs {mode = Menu}
    Quit -> gs { gameActive = False}
    _ -> gs

handleIngameEvents (x:xs) gs = handleIngameEvents xs (handleIngameEvent [x] gs)
handleIngameEvents [] gs = gs


