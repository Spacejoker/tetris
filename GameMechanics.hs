module GameMechanics where

import System.Random
import Model
import Physics

-- Handles game commands including the entire tick-step in gamestate

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
	    merged = merge (getBlockPositions gs) (field gs)
	    score' = getScoreForClear (length (getRmLines merged))
	    fld = clearDone  merged
	    gs' = genNewBlock gs
        in gs' { field = fld, steps = 0, score = (score gs') + score'}

--generates an endless list of random pieces
createRandomList :: StdGen -> [(Int, StdGen)]
createRandomList gen = 
  let x = pieceR gen 
  in (x : createRandomList (snd x))

genNewBlock :: GameState -> GameState
genNewBlock gs =
  let (val, gen') = pieceR (gen gs)
      queue' = tail (queue gs) ++ [val]
      b = Block 0 0 0 (head (queue gs))
      gs' = gs {gen = gen', block = b, queue = queue'}
  in case legalPosition 0 0 gs' of 
    True -> gs'
    _ -> gs { lost = True } -- player loose
 
makeBlks :: [(Int, Int)] -> Clr -> [Blk]
makeBlks [x] clr = [Blk x clr]
makeBlks (x:xs) clr = Blk x clr: makeBlks xs clr

clearDone ::[Blk] -> [Blk]
clearDone fld =
  let 
      rmLines = getRmLines fld
      fld' = clearLines fld rmLines
      fld'' = shiftLines fld' rmLines
      in fld''

shiftLines [] _ = []
shiftLines (Blk (a,b) c:xs) lst =
  let nrShifts = length $ filter (>b) lst
  in (Blk (a, b +nrShifts) c:shiftLines xs lst)

getRmLines :: [Blk] -> [Int]
getRmLines fld =
      let cnt = countElementPerLine fld
          rmLines = getRmLines' 0 cnt
      in rmLines

getRmLines' :: Int -> [Int] -> [Int]
getRmLines' _ [] = []
getRmLines' curLine (x : xs) =
  case x >= width of
    True -> (curLine : getRmLines' (curLine +1)  xs)
    otherwise -> getRmLines' (curLine+1) xs

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

getBlockPositions gs = 
  let b = block gs
      curBlock = blocks !! (blockId b)
  in map (addPosition (x b) (y b)) (curBlock  !! (rot b))

getScoreForClear :: Int -> Int
getScoreForClear numRows =
  case numRows of
    1 -> 100
    2 -> 200
    3 -> 400
    4 -> 800
    _ -> 0
