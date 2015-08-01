module CustomTypes where
import Data.Array

data Color = Black | White
  deriving (Eq, Show)

data Move = Pass | Play (Int, Int)
  deriving (Eq, Show)

data Tile = Mine | Opponent | Neutral
  deriving (Eq, Show)

-- data structure of the game tree nodes
data Node = Node Board Player Coordinates
  deriving (Eq, Show)

type Player = Tile
type Coordinates = (Int,Int)
type Board = Array Coordinates Tile 
type State = Board