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
	  queue :: [Int]
        }

data GraphicsProvider = GraphicsProvider {
	  
	}

data GameMode = Menu | GamePlay | Credits
data Clr = Blue | Red

data Blk = Blk {
	  pos :: (Int, Int),
	  color :: Clr
	}

blocks = [blockI, blockJ, blockT, blockO, blockZ, blockS, blockL]

blockO = [[Blk (1,1) Red, Blk (2,1) Red ,Blk (2,2) Red ,Blk (1,2) Red ]] :: [[Blk]]
blockI = [[Blk (2,0) Red ,Blk (2,1) Red ,Blk (2,2) Red ,Blk (2,3) Red ],[Blk (0,1) Red ,Blk (1,1) Red ,Blk (2,1) Red ,Blk (3,1) Red ]] :: [[Blk]]
blockS = [[Blk (1,2) Red ,Blk (2,1) Red ,Blk (2,2) Red ,Blk (3,1) Red ],[Blk (2,0) Red ,Blk (2,1) Red ,Blk (3,1) Red ,Blk (3,2) Red ]] :: [[Blk]]
blockZ = [[Blk (1,1) Red ,Blk (2,1) Red ,Blk (2,2) Red ,Blk (3,2) Red ],[Blk (3,0) Red ,Blk (2,1) Red ,Blk (3,1) Red ,Blk (2,2) Red ]] :: [[Blk]]
blockL = [[Blk (1,1) Red ,Blk (1,2) Red ,Blk (2,1) Red ,Blk (3,1) Red ],[Blk (2,0) Red ,Blk (3,2) Red ,Blk (2,1) Red ,Blk (2,2) Red ],[Blk (3,0) Red ,Blk (1,1) Red ,Blk (2,1) Red ,Blk (3,1) Red ],[Blk (2,0) Red ,Blk (2,1) Red ,Blk (2,2) Red ,Blk (1,0) Red ]] :: [[Blk]]
blockJ = [[Blk (2,0) Red ,Blk (2,1) Red ,Blk (2,2) Red ,Blk (1,2) Red ],[Blk (1,1) Red ,Blk (2,1) Red ,Blk (3,1) Red ,Blk (3,2) Red ],[Blk (2,0) Red ,Blk (3,0) Red ,Blk (2,1) Red ,Blk (2,2) Red ],[Blk (1,0) Red ,Blk (1,1) Red ,Blk (2,1) Red ,Blk (3,1) Red ]] :: [[Blk]]
blockT = [[Blk (1,1) Red ,Blk (2,1) Red ,Blk (3,1) Red ,Blk (2,2) Red ],[Blk (2,0) Red ,Blk (2,1) Red ,Blk (2,2) Red ,Blk (3,1) Red ],[Blk (1,1) Red ,Blk (2,1) Red ,Blk (3,1) Red ,Blk (2,0) Red ],[Blk (1,1) Red ,Blk (2,1) Red ,Blk (2,0) Red ,Blk (2,2) Red ]] :: [[Blk]]



