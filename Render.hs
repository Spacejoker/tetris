module Render where
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import System.Random

import Model
import Physics
import GameMechanics

renderMenu :: GameState -> IO ()
renderMenu gs = do
  s <- getVideoSurface

  let gr = (graphics gs)
  blitSurface (menubg gr) Nothing s (Just (Rect 0 0 800 600))

  title <- renderTextSolid (font gs) "Jens Mega Haskell Tetris" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 330 50 200 400))
  
  let animframe = (floor ((fromIntegral (steps gs))/10.0)) `mod` (length (newGame gr)) 

  let newimg = if menuchoice gs == 0 then  ((newGame gr) !! animframe ) else (newGame gr) !! 0
  blitSurface newimg Nothing s (Just (Rect 235 300 350 400))

  let creditimg = if menuchoice gs == 1 then  ((credits gr) !! animframe ) else (credits gr) !! 0
  blitSurface creditimg Nothing s (Just (Rect 245 370 350 460))

  let quitimg = if menuchoice gs == 2 then  ((quits gr) !! animframe ) else (quits gr) !! 0
  blitSurface quitimg Nothing s (Just (Rect 245 440 350 520))

  let markery = 314+(menuchoice gs)*70
  blitSurface (cm gr)  Nothing s (Just (Rect 180 markery 350 520))
  blitSurface (cm gr)  Nothing s (Just (Rect 594 markery 350 520))

  SDL.flip s

render :: GameState -> IO ()
render gs = do 

  s <- getVideoSurface

  let gr = (graphics gs)
  blitSurface (bg gr) Nothing s (Just (Rect 0 0 800 600))

  title <- renderTextSolid (font gs) "Score" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 480 60 200 40))
  
  title <- renderTextSolid (font gs) ((show (score gs))) (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 610 105 200 40))
  
  paint' leftOffset topOffset (merge (getBlockPositions gs) (field gs)) s gs

  paintQueue (queue gs) 0 s gs

  lostText <- renderTextSolid (font gs) "GAME OVER" (Color 255 0 0)
  case lost gs of
    True -> blitSurface lostText Nothing s (Just (Rect 350 300 200 40))
    _ -> return False

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
  let blks = blockGraphics $ (graphics gs)
  in case  color blk of 
     Red -> blks !! 0
     Blue -> blks !! 1
     Orange -> blks !! 2
     Violet -> blks !! 3
     Green -> blks !! 4
     Yellow -> blks !! 5
     _ -> blks !! 6
