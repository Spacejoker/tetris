module Model where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

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
	  field :: [(Int, Int, Int)] -- (x, y, value)
        }

blocks = [blockI, blockJ, blockT, blockO, blockZ, blockS, blockL]

blockO = [[(1,1,1),(2,1,1),(2,2,1),(1,2,1)]] :: [[(Int, Int, Int)]]
blockI = [[(2,0,1),(2,1,1),(2,2,1),(2,3,1)],[(0,1,1),(1,1,1),(2,1,1),(3,1,1)]] :: [[(Int, Int, Int)]]
blockS = [[(1,2,1),(2,1,1),(2,2,1),(3,1,1)],[(2,0,1),(2,1,1),(3,1,1),(3,2,1)]] :: [[(Int, Int, Int)]]
blockZ = [[(1,1,1),(2,1,1),(2,2,1),(3,2,1)],[(3,0,1),(2,1,1),(3,1,1),(2,2,1)]] :: [[(Int, Int, Int)]]
blockL = [[(1,1,3),(1,2,3),(2,1,3),(3,1,3)],[(2,0,3),(3,2,3),(2,1,3),(2,2,3)],[(3,0,3),(1,1,3),(2,1,3),(3,1,3)],[(2,0,3),(2,1,3),(2,2,3),(1,0,3)]] :: [[(Int, Int, Int)]]
blockJ = [[(2,0,2),(2,1,2),(2,2,2),(1,2,2)],[(1,1,2),(2,1,2),(3,1,2),(3,2,2)],[(2,0,2),(3,0,2),(2,1,2),(2,2,2)],[(1,0,2),(1,1,2),(2,1,2),(3,1,2)]] :: [[(Int, Int, Int)]]
blockT = [[(1,1,2),(2,1,2),(3,1,2),(2,2,2)],[(2,0,2),(2,1,2),(2,2,2),(3,1,2)],[(1,1,2),(2,1,2),(3,1,2),(2,0,2)],[(1,1,2),(2,1,2),(2,0,2),(2,2,2)]] :: [[(Int, Int, Int)]]



