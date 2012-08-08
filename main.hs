import Graphics.UI.SDL as SDL

data Block = Block {
	  position :: Int  
	}

data GameState = GameState{
          gameActive :: Bool,
          score :: Int,
          block :: Block
        }

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []

  setCaption "Tetris deluxe" "efefef" 

  enableKeyRepeat 500 30

  gameLoop (GameState True 0 (Block 0)) 

gameLoop :: GameState -> IO ()
gameLoop gs = do
  events <- getEvents pollEvent []

  let gs' = handleEvents events gs

  putStrLn $ show $ position $ block $ gs

  delay 10 

--  render gs'
 
  if (gameActive gs')
    then gameLoop  gs' 
    else quit'


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
    _ -> gs

handleEvents (x:xs) gs = handleEvents xs (handleEvents [x] gs)
handleEvents [] gs = gs


move :: Int -> GameState -> GameState
move d gs = gs'
  where gs' = gs { block = b}
        prevPos = position $ block $ gs
        b = Block (prevPos + d)
