import Graphics.UI.SDL as SDL

data GameState = GameState{
          gameActive :: Bool,
          score :: Int
        }

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []

  setCaption "Tetris deluxe" "efefef" 

  enableKeyRepeat 500 30

  gameLoop (GameState True 0) 

gameLoop :: GameState -> IO ()
gameLoop gs = do
  events <- getEvents pollEvent []
  putStrLn $ show $ length events

  let gs' = handleEvents events gs

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
handleEvents ::(Show a) => [a] -> GameState ->  GameState
handleEvents [x] gs = gs
handleEvents (x:xs) gs = 
  gs { gameActive = False }
handleEvents [] gs = gs



