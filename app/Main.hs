module Main where

import Game
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import Logic
import Render
import System.Random (getStdGen, newStdGen)

main :: IO ()
main =  do
    g <- getStdGen
    h <- newStdGen
    play
        window
        windowBackground
        60
        (initGame g h)
        renderApp
        handleInput
        update

handleInput :: Event -> Game -> Game
handleInput (EventKey (MouseButton LeftButton) Up _ mousePos)  game = game {
    playing = (mousePositionAsCellPos mousePos, Just LeftB)
    }
handleInput (EventKey (MouseButton RightButton) Up _ mousePos) game = game {
    playing = (mousePositionAsCellPos mousePos, Just RightB)
    }
handleInput _                                                  game = game
