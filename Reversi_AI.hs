-- Authors:
-- Martin Mihaylov (https://github.com/martomi)
-- Georgi Dikov (https://github.com/gdikov)

module Reversi_AI (State, Tile (Mine,Opponent,Neutral), startState, nextState) where
import CustomTypes
import Data.Array
import Data.Maybe

-----------------------------------------------
-- Library and other definitions 
-----------------------------------------------

type Direction  = Coordinates
up        = (0,-1)  :: Direction
upRight   = (1,-1)  :: Direction
right     = (1, 0)  :: Direction
downRight = (1, 1)  :: Direction
down      = (0, 1)  :: Direction
downLeft  = (-1,1)  :: Direction
left      = (-1, 0) :: Direction
upLeft    = (-1,-1) :: Direction

directions :: [Direction]
directions  = [up, upRight, right, downRight, down, downLeft, left, upLeft]

recursionDepth :: State -> Int
recursionDepth state
  | countAllChips state < 12 = 2  -- opening
  | countAllChips state > 51 = 15 -- endgame
  | otherwise = 4                 -- midgame

aiIsBlack_Board :: Board
aiIsBlack_Board = (array ((0,0),(7,7)) [((x,y),(Neutral)) | x<-[0..7], y<-[0..7]]) // 
  [((3,3),Opponent),((3,4),Mine),((4,3),Mine),((4,4),Opponent)]
    
aiIsWhite_Board :: Board
aiIsWhite_Board = (array ((0,0),(7,7)) [((x,y),(Neutral)) | x<-[0..7], y<-[0..7]]) // 
  [((3,3),Mine),((3,4),Opponent),((4,3),Opponent),((4,4),Mine)]

-----------------------------------------------
-- Initial state and calculation of new states
-----------------------------------------------

startState :: Color -> State
startState whoAmI
  | whoAmI == Black = aiIsBlack_Board
  | otherwise = aiIsWhite_Board

nextState :: State -> Move -> (Move, State)
nextState oldState opponentMove = (myMove , playMove actualState myMove Mine)
  where
    myMove = findBestMove actualState Opponent opponentMove -- Opponent was the last to place chip on this state
    actualState = playMove oldState opponentMove Opponent 

-----------------------------------------------
-- Basic functions
-----------------------------------------------

isOpp :: Player -> State -> Coordinates -> Bool
isOpp player state coord = (state ! coord) == (oppPlayer player)

isMine :: Player -> State -> Coordinates -> Bool
isMine player state coord = (state ! coord) == player

isNeutral :: State -> Coordinates -> Bool
isNeutral state coord = (state ! coord) == Neutral

oppPlayer :: Tile -> Tile 
oppPlayer Mine = Opponent
oppPlayer Opponent = Mine 

goThisDir :: Coordinates -> Direction -> Maybe Coordinates
goThisDir (c1,c2) (d1,d2)
  | x > 7 || x < 0 || y > 7 || y < 0 = Nothing
  | otherwise = Just (x, y)
    where 
        x = c1 + d1
        y = c2 + d2

countChips :: State -> Player -> Int
countChips state player =
  foldl counter 0 [(x,y)| x <- [0..7], y <- [0..7]]
  where
    counter n coord
      | state ! coord == player = n + 1
      | otherwise = n

countAllChips :: State -> Int
countAllChips state =
  foldl counter 0 [(x,y)| x <- [0..7], y <- [0..7]]
  where
    counter n coord
      | state ! coord /= Neutral = n + 1
      | otherwise = n

-----------------------------------------------
-- Function for playing a move
-----------------------------------------------

playMove :: State -> Move -> Player -> State
playMove state Pass _ = state
playMove state (Play coord) player = playOnCoord state player coord

playOnCoord :: State -> Player -> Coordinates -> State
playOnCoord state player (x,y) = flipChips player newState (x,y)
  where newState = placeChip (x,y) state player -- sets the chip

-- returns a list of all possible coordinates, where a coin could be placed
availableMoves :: State -> Player -> [Coordinates]
availableMoves state player = filter (isLegal state player) freeCoord
  where freeCoord = [(x,y) | x <- [0..7], y <- [0..7], isNeutral state (x,y)]

-- Position is legal when there are Chips wich can be flipped by placing a our chip on this position
isLegal :: State -> Player -> Coordinates -> Bool
isLegal state player coord = (length listOfCoordToFlip) > 0
  where listOfCoordToFlip = coordToFlip player state coord

placeChip :: Coordinates -> State -> Tile -> State
placeChip coord board chip = board // [(coord, chip)]

-----------------------------------------------
-- Function for Flipping Chips
-----------------------------------------------

flipChips :: Player -> State -> Coordinates -> State
flipChips player state = (flipIt state) . (coordToFlip player state)

-- Finds the list of all chips which are to be flipped
coordToFlip :: Player -> State -> Coordinates -> [Coordinates]
coordToFlip player state coord = foldl (oneDimChange player coord state []) [] directions

-- Checks whether something can be flipped in a single direction
oneDimChange :: Player -> Coordinates -> State -> [Coordinates] -> [Coordinates] -> Direction -> [Coordinates]
oneDimChange player coord state coordMaybeToChange coordToFlip direction
  | maybeNewCoord == Nothing = coordToFlip
  | isNeutral state newCoord = coordToFlip
  | isOpp player state newCoord = oneDimChange player newCoord state (newCoord:coordMaybeToChange) coordToFlip direction
  | otherwise = coordToFlip ++ coordMaybeToChange
    where 
      maybeNewCoord = goThisDir coord direction
      newCoord = fromJust maybeNewCoord

-- Flips all Tiles wich are specified by the list of coordinates
flipIt :: State -> [Coordinates] -> State
flipIt state [] = state
flipIt state (c:cs) = state // [(coord, changedColor) | coord <- (c:cs)]
  where changedColor = oppPlayer (state ! c)

-----------------------------------------------
-- Game Tree functions
-----------------------------------------------

isTerminal :: Node -> Bool
isTerminal (Node board player coord) = (availableMoves board (oppPlayer player) == []) && (availableMoves board player == [])

evalNode :: Node -> Int
evalNode  (Node board player coord) = evaluateHeurisitically board coord player

getChildren :: Node -> [Node]
getChildren  (Node board player coord)
  | moves == [] = [(Node board changedPlayer (10,10))] -- Coordinates 10,10 <=> Pass
  | otherwise = generateNode childBoards changedPlayer moves
    where
      changedPlayer = oppPlayer player
      childBoards = map (playOnCoord board changedPlayer) moves
      moves = availableMoves board changedPlayer

generateNode :: [Board] -> Player -> [Coordinates] -> [Node]
generateNode [] _ [] = []
generateNode (b:boards) player (c:coords) = (Node b player c) : (generateNode boards player coords)

-----------------------------------------------
-- NegaMax with Alpha Beta Pruning
-- http://en.wikipedia.org/wiki/Negamax#NegaMax_with_Alpha_Beta_Pruning
--
-- Could be further optimised with Transposition Tables
-- http://en.wikipedia.org/wiki/Negamax#NegaMax_with_Alpha_Beta_Pruning_and_Transposition_Tables
-----------------------------------------------

alphabeta :: Node -> Int -> Int -> Int -> ([Node],Int)
alphabeta node depth alpha beta
  | depth == 0 || isTerminal node = ([node], evalNode node) -- evalNode takes care of sign (+/-)
  | otherwise = (node:nodes, value)
        where (nodes, value) = negamax (getChildren node) depth alpha beta ([], (minBound :: Int)+1)

negamax [] _ _ _ bestValue = bestValue -- no more children to examine
negamax (child:children) depth alpha beta (nodes, value)
  | alpha >= beta = bestValue
  | otherwise = negamax children depth alphaMax beta bestValue
    where 
      (newNodes, newValue) = negate $ alphabeta child (depth-1) (-beta) (-alphaMax)
      bestValue = if (newValue) > value then (newNodes, newValue) else (nodes, value)
      alphaMax = max alpha value
      negate (nodes, value) = (nodes,-value)

-----------------------------------------------
-- Heuristic, evaluation and analysis functions
-----------------------------------------------

findBestMove :: State -> Player -> Move -> Move
findBestMove state player move = if (tail variation) == [] 
                                    then Pass -- end of game
                                    else if (getCoords.head.tail $ variation) == (10,10) 
                                    then Pass -- could not find possible moves
                                    else Play (getCoords.head.tail $ variation)
  where 
    (variation, score) = alphabeta (makeNode state player move) (recursionDepth state) (minBound :: Int) (maxBound :: Int)
    makeNode state player Pass = (Node state player (10,10))
    makeNode state player (Play coord) = (Node state player coord)

getCoords :: Node -> Coordinates
getCoords (Node _ _ coord) = coord

evaluateHeurisitically :: State -> Coordinates -> Player -> Int
evaluateHeurisitically state coord player 
  | player == Mine = (chipHeurValue + mobilityHeurValue + positionalHeurValue + stabilityHeurValue) 
  | otherwise = -(chipHeurValue + mobilityHeurValue + positionalHeurValue + stabilityHeurValue)
    where
      numChips = countAllChips state

      chipHeurValue 
        | numChips <= 12 = (-100)*diffChips    -- opening
        | numChips <= 63 = 100*diffChips   -- midgame
        | otherwise = 10000*diffChips
          where 
            myChips = countChips state player
            oppChips = numChips - myChips
            diffChips = myChips - oppChips

      mobilityHeurValue  -- expected values = [0;1500]
        | numChips <= 12 = 300*(myMoves - oppMoves)                    -- opening
        | numChips <= 63 = 100*(myMoves - oppMoves)   -- pre-endgame & endgame
        | otherwise = 0 -- in the last moves, worth is only the number of chips
          where 
            myMoves = length $ availableMoves state player
            oppMoves = length $ availableMoves state (oppPlayer player)

      positionalHeurValue = (goodChips-badChips)
          where 
            goodChips = sum [evalChip state numChips player (x,y) | x<- [0..7], y<- [0..7]]
            badChips = sum [evalChip state numChips (oppPlayer player) (x,y) | x<- [0..7], y<- [0..7]]

      stabilityHeurValue
        | numChips <= 46 = 0 
        | otherwise = 8000*stableChips  
          where
            stableChips = 
              ((length [ x | x<-[0..7], isOpp player state (x,0)] `mod` 7) `div` 6) +
              ((length [ x | x<-[0..7], isOpp player state (x,7)] `mod` 7) `div` 6) +
              ((length [ y | y<-[0..7], isOpp player state (0,y)] `mod` 7) `div` 6) +
              ((length [ y | y<-[0..7], isOpp player state (7,y)] `mod` 7) `div` 6)

evalChip :: State -> Int -> Player -> Coordinates -> Int
evalChip state numChips player coord
  | isMine player state coord = (weightedBoard ! coord)
  | otherwise = 0
    where 
      weightedBoard
        | numChips <= 50 = staticWeightedBoard
        | otherwise = staticWeightedBoardFinal

staticWeightedBoard :: Array Coordinates Int
staticWeightedBoard = listArray ((0,0),(7,7))
  [10000,-3000,1000,800,800,1000,-3000,10000,
  -3000,-5000,-450,-500,-500,-450,-5000,-3000,
  1000,-450,30,10,10,30,-450,1000,
  800,-500,10,50,50,10,-500,800,
  800,-500,10,50,50,10,-500,800,
  1000,-450,30,10,10,30,-450,1000,
  -3000,-5000,-450,-500,-500,-450,-5000,-3000,
  10000,-3000,1000,800,800,1000,-3000,10000]

staticWeightedBoardFinal :: Array Coordinates Int
staticWeightedBoardFinal = listArray ((0,0),(7,7))
  [10000,-3000,1000,800,800,1000,-3000,10000,
  -3000,-3050,-450,-500,-500,-450,-3050,-3000,
  1000,-450,30,10,10,30,-450,1000,
  800,-500,10,50,50,10,-500,800,
  800,-500,10,50,50,10,-500,800,
  1000,-450,30,10,10,30,-450,1000,
  -3000,-3050,-450,-500,-500,-450,-3050,-3000,
  10000,-3000,1000,800,800,1000,-3000,10000]