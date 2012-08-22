module Model where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import System.Random

-- Game Constants
dim = 24 :: Int 
width = 10 :: Int
height = 22 :: Int
leftOffset = 55 :: Int 
topOffset = 35 :: Int
ticksPerStep = 50 :: Int 

data Block = Block {
	  x :: Int,
	  y :: Int,
	  rot :: Int,
	  blockId :: Int
	}

data GameState = GameState{
	  gameActive ::	Bool,
	  score :: Int,
	  block :: Block,
	  font :: Font,
	  steps :: Int,
	  field :: [Blk],
	  gen :: StdGen,
	  mode :: GameMode,
	  queue :: [Int],
	  graphics :: Graphics,
	  lost :: Bool,
	  menuchoice :: Int
        }

data Graphics = Graphics{	
	  bg :: Surface,
	  blockGraphics :: [Surface],
	  menubg :: Surface,
	  newGame :: [Surface],
	  credits :: [Surface],
	  quits :: [Surface],
	  cm :: Surface
	}

data GameMode = Menu | GamePlay | Credits
data Clr = Blue | Red | Orange | Green | Yellow | Cyan | Violet
  deriving (Eq)

data Blk = Blk {
	  pos :: (Int, Int),
	  color :: Clr
	}

blocks = [blockI, blockJ, blockT, blockO, blockZ, blockS, blockL]

blockO = [[Blk (1,1) Red, Blk (2,1) Red ,Blk (2,2) Red ,Blk (1,2) Red ]] :: [[Blk]]
blockI = [[Blk (2,0) Blue ,Blk (2,1) Blue ,Blk (2,2) Blue ,Blk (2,3) Blue ],[Blk (0,1) Blue ,Blk (1,1) Blue ,Blk (2,1) Blue ,Blk (3,1) Blue ]] :: [[Blk]]
blockS = [[Blk (1,2) Orange ,Blk (2,1) Orange ,Blk (2,2) Orange ,Blk (3,1) Orange ],[Blk (2,0) Orange ,Blk (2,1) Orange ,Blk (3,1) Orange ,Blk (3,2) Orange ]] :: [[Blk]]
blockZ = [[Blk (1,1) Green ,Blk (2,1) Green ,Blk (2,2) Green ,Blk (3,2) Green ],[Blk (3,0) Green ,Blk (2,1) Green ,Blk (3,1) Green ,Blk (2,2) Green ]] :: [[Blk]]
blockL = [[Blk (1,1) Yellow ,Blk (1,2) Yellow ,Blk (2,1) Yellow ,Blk (3,1) Yellow ],[Blk (2,0) Yellow ,Blk (3,2) Yellow ,Blk (2,1) Yellow ,Blk (2,2) Yellow ],[Blk (3,0) Yellow ,Blk (1,1) Yellow ,Blk (2,1) Yellow ,Blk (3,1) Yellow ],[Blk (2,0) Yellow ,Blk (2,1) Yellow ,Blk (2,2) Yellow ,Blk (1,0) Yellow ]] :: [[Blk]]
blockJ = [[Blk (2,0) Cyan ,Blk (2,1) Cyan ,Blk (2,2) Cyan ,Blk (1,2) Cyan ],[Blk (1,1) Cyan ,Blk (2,1) Cyan ,Blk (3,1) Cyan ,Blk (3,2) Cyan ],[Blk (2,0) Cyan ,Blk (3,0) Cyan ,Blk (2,1) Cyan ,Blk (2,2) Cyan ],[Blk (1,0) Cyan ,Blk (1,1) Cyan ,Blk (2,1) Cyan ,Blk (3,1) Cyan ]] :: [[Blk]]
blockT = [[Blk (1,1) Violet ,Blk (2,1) Violet ,Blk (3,1) Violet ,Blk (2,2) Violet ],[Blk (2,0) Violet ,Blk (2,1) Violet ,Blk (2,2) Violet ,Blk (3,1) Violet ],[Blk (1,1) Violet ,Blk (2,1) Violet ,Blk (3,1) Violet ,Blk (2,0) Violet ],[Blk (1,1) Violet ,Blk (2,1) Violet ,Blk (2,0) Violet ,Blk (2,2) Violet ]] :: [[Blk]]



