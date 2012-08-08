module Model where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

data Block = Block {
	  x :: Int,
	  y :: Int,
	  rot :: Int
	}

data GameState = GameState{
	  gameActive ::	Bool,
	  score :: Int,
	  block :: Block,
	  font :: Font,
	  steps :: Int
        }

blocks = [blockI]


blockI = [[(2,0),(2,1),(2,2),(2,3)],[(0,1),(1,1),(2,1),(3,1)]] :: [[(Int, Int)]]
