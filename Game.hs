module Game where

import Data.Array

data Player = PlayerX | PlayerY deriving (Eq, Show)
data State = InProgress | Stopped (Maybe Player) deriving (Eq, Show)
type Cell = Maybe Player

type Board = Array (Int, Int) Cell

data GameState = GameState {
    board :: Board,
    player :: Player,
    state :: State
    } 
    deriving (Eq, Show)

firstGame = GameState {
    board = array points $ zip (range points) (cycle [Nothing]),
    player = PlayerX,
    state = InProgress 
    }
    where points = ((0, 0), (2, 2))