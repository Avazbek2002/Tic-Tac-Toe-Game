module Rendering where

import Data.Array
import Graphics.Gloss
import Game

windowHeight = 800
windowWidth = 600

cellHeight = windowHeight/3
cellWidth = windowWidth/3

inProgressBoard board = Blank

stoppedBoard winner board = color (finalColor winner) (frameBoard board)

frameBoard board = 
    pictures [
        xCells board,
        oCells board,
        grid
    ]

grid :: Picture
grid = 
    pictures
    $ concatMap (\i -> [line [(i * cellWidth, 0.0),
    (i * cellWidth, windowHeight)
    ],
    line [(i*cellHeight, 0.0),
    (i*cellHeight, windowWidth)]
    ])
    [0.0 .. 3.0]

xCells :: Board -> Picture
xCells board = cells board (Just PlayerX) xCell

oCells :: Board -> Picture
oCells board = cells board (Just PlayerY) oCell

xCell :: Picture
xCell = pictures [
    rotate (45.0) $ rectangleSolid side 10.0,
    rotate (-45.0) $ rectangleSolid side 10.0
    ]
    where side = min cellWidth cellHeight * 0.75

oCell :: Picture
oCell = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellHeight * 0.5
          y = fromIntegral row * cellHeight + cellWidth * 0.5 

cells :: Board -> Cell -> Picture -> Picture
cells board cell pictureOfCell = 
    pictures
    $ map (snapPictureToCell pictureOfCell . fst)
    $ filter isCell 
    $ assocs board
    where isCell (_, e) = e == cell
finalColor :: Cell -> Color
finalColor (Just PlayerX) = playerXColor
finalColor (Just PlayerY) = playerYColor
finalColor Nothing = noColor

playerXColor = makeColor 0 0 255 0 -- blue color
playerYColor = makeColor 255 0 0 0 -- red color
noColor = greyN 0.5

gameAsPicture :: GameState -> Picture
gameAsPicture game = translate (windowWidth * (-0.5)) (windowHeight * (-0.5)) frame
    where frame = case state game of
                            InProgress -> inProgressBoard (board game)
                            Stopped winner -> stoppedBoard winner (board game)