module Logic where

import Data.Array
import Game
import Rendering
import Graphics.Gloss.Interface.Pure.Game

switchPlayer :: GameState -> GameState
switchPlayer game
    | (player game) == PlayerX = game {player = PlayerY}
    | otherwise              = game {player = PlayerX} 

playerTurn :: GameState -> (Int, Int) -> GameState
playerTurn game (x, y)
    | isCorrectCoordinate (x, y) && (board game) ! (x, y) == Nothing =
      isStopped
      $ switchPlayer 
      $ game {board = (board game) // [((x, y), Just $ (player game))]}
    | otherwise = game

isCorrectCoordinate = inRange ((0, 0), (2, 2))

isPlayerWon :: Player -> Board -> Bool
isPlayerWon player board = any isVictory special
    where special = rows ++ columns ++ diags
          rows = [[(j, i) | j <- [0 .. 2]] | i <- [0 .. 2]]
          columns = [[(i, j) | i <- [0 .. 2]] | j <- [0 .. 2]]
          diags = [
                    [(i, i) | i <- [0 .. 2]],
                    [(i, 2 - i) | i <- [0 .. 2]]
                  ]
          isVictory proj = (3==) $
                              length $
                              filter (\cell -> cell == Just player) $
                              map (\coord -> board ! coord) proj

countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

isStopped game
    | isPlayerWon PlayerX $ board game = game {state = Stopped $ Just PlayerX}
    | isPlayerWon PlayerY $ board game = game {state = Stopped $ Just PlayerY}
    | countCells Nothing (board game) == 0 = game {state = Stopped Nothing}
    | otherwise                 = game
           
mouseOnGrid :: (Float, Float) -> (Int, Int)
mouseOnGrid (x, y) = (a, b)
    where a = floor (x + (windowHeight * 0.5) / cellWidth)
          b = floor (y + (windowWidth * 0.5) / cellHeight)

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case state game of
        InProgress -> playerTurn game $ mouseOnGrid mousePos
        Stopped _ -> firstGame
transformGame _ game = game