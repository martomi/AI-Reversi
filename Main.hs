-- Creates a very simple Reversi game where you can play against the AI.
-- Just for demonstration purposes. No input validation is implemented!

module Main where
import Reversi_AI as AI
import CustomTypes
import Data.Array
import Data.Maybe
import Data.List

main :: IO ()
main = do
    putStr $ boardToAscii (AI.startState White)
    gameLoop (AI.startState White)
    where 
        gameLoop gameState = do
            putStr "You are playing with (X). Enter coordinates:\n"
            x <- getLine
            y <- getLine
            newGameState <- return $ snd $
                if ((read(x)::Int) == 10 && (read(y)::Int) == 10) then
                    AI.nextState gameState Pass
                else
                    AI.nextState gameState (Play (read(x)::Int,read(y)::Int))
            putStr $ boardToAscii newGameState
            putStr "Your score: "
            putStr $ show $ countScore newGameState Opponent
            putStr "\tScore AI: "
            putStrLn $ show $ countScore newGameState Mine
            gameLoop newGameState

countScore :: State -> Tile -> Int
countScore state player = length [(x,y) | x<- [0..7], y<-[0..7], (state ! (x,y))==player] 

countAllChips :: State -> Int
countAllChips state =
  foldl counter 0 [(x,y)| x <- [0..7], y <- [0..7]]
  where
    counter n coord
      | state ! coord /= Neutral = n + 1
      | otherwise = n

-- String representation of the game state
-- Source: https://github.com/geon/Othello-in-Haskell
boardToAscii :: State -> String
boardToAscii board =
    "\n  0 1 2 3 4 5 6 7 \n +-+-+-+-+-+-+-+-+\n" ++
    (intercalate
        "\n +-+-+-+-+-+-+-+-+\n"
        (map (boardToRow board) [0..7])
    ) ++
    "\n +-+-+-+-+-+-+-+-+\n  0 1 2 3 4 5 6 7 \n"

boardToRow :: State -> Int -> String
boardToRow board row = 
    show row ++ "|" ++
    (intercalate "|" 
        (map (\position -> colorToAscii (board ! position)) ([(x, row) | x <- [0..7]]))
    ) ++ "|" ++ show row

colorToAscii :: Tile -> String
colorToAscii color = case color of
    Neutral -> " "
    Mine -> "O"
    Opponent -> "X"