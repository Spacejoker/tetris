module Physics where

import Model

addPosition x y (Blk (a,b) clr) = Blk (a+x, b+y) clr

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
