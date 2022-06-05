module Render where

import           Data.Array
import           Game
import           Graphics.Gloss hiding (text)
import qualified Graphics.Gloss as G (text)
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Interface.Pure.Game hiding (text)

testMenu :: State -> IO ()
testMenu s = display window windowBackground (renderMenu Menu)

-- Window parameters -------------------------------------------------------------------------
window :: Display
window = InWindow "Functional MineSweeper" initialWindowSize windowPosition

initialWindowSize :: (Int, Int)
initialWindowSize = (smallWindowWidth, smallWindowHeight)

smallWindowWidth :: Int
smallWindowWidth = 380

smallWindowHeight :: Int
smallWindowHeight = 500

smallGridSize :: (Int, Int)
smallGridSize = (340, 340)

headerSize :: (Int, Int)
headerSize = (fst smallGridSize, 80)

windowPosition :: (Int, Int)
windowPosition = (1920, 100)

cellSize :: Float
cellSize = 42.5

-- Color ------------------------------------------------------------------------------------

boxBackground :: Color
boxBackground = makeColorI 69 77 102 255


windowBackground :: Color
windowBackground = makeColorI 0 153 117 255

groundColor :: Color
groundColor = makeColorI 89 179 105 255

dugColor :: Color
dugColor = makeColorI 217 216 114 255

-- App rendering -----------------------------------------------------------------------------

renderApp :: Game -> Picture
renderApp g
    | s == Running     = renderGame g
    | s == GameOver    = pictures [ color red $ renderGame g]
    | s == Menu        = renderMenu s
    | s == LevelSelect = renderMenu s
    | otherwise        = Blank
    where s = state g

-- Game rendering ----------------------------------------------------------------------------

renderGame :: Game -> Picture
renderGame game = pictures
    [ translate 0.0 vPos renderMainContainer
    , translate (-x/2 + cellSize/2) (-x/2 - cellSize + 4.5) $ color groundColor $ renderMinefield game
    , renderHeader "Game"
    ]
    where vPos  = - (fromIntegral smallWindowHeight/2 - y/2 - 20.0)
          x        = fromIntegral $ fst smallGridSize
          y        = fromIntegral $ snd smallGridSize

renderMainContainer :: Picture
renderMainContainer = pictures [
      color (bright boxBackground) $ rectangleSolid (x + 20) (y + 20)
    , color (dim boxBackground) $ rectangleSolid (x + 6) (y + 6)
    , color boxBackground $ rectangleSolid x y
    ]
    where x = fromIntegral $ fst smallGridSize
          y = fromIntegral $ snd smallGridSize

renderMinefield :: Game -> Picture
renderMinefield g = pictures [
      renderUnsureCell mf
    , renderDugCell    mf
    , renderGroundCell mf
    , renderFlagCell   mf
    ]
    where mf = minefield g

renderDugCell :: Minefield -> Picture
renderDugCell mf = pictures [
    cellsOfMinefield mf Dug    dugPict
  , cluesOfMinefield mf ]

renderFlagCell :: Minefield -> Picture
renderFlagCell mf =
    cellsOfMinefield mf Flag   flagPict

renderUnsureCell :: Minefield -> Picture
renderUnsureCell mf =
    cellsOfMinefield mf Unsure unsurePict

renderGroundCell :: Minefield  -> Picture
renderGroundCell mf =
    cellsOfMinefield mf Ground groundPict

cellsOfMinefield :: Minefield -> Cell -> Picture -> Picture
cellsOfMinefield mf c p = pictures
    $ map (snapToGrid p . fst)
    $ filter (\(_,(cell, _)) -> cell == c)
    $ assocs mf

cluesOfMinefield :: Minefield -> Picture
cluesOfMinefield mf = pictures
    $ map (\(coord, (_, clue)) -> snapToGrid (getCluePict clue) coord)
    $ filter (\(_,(cell, clue)) -> cell == Dug && clue /= Empty)
    $ assocs mf

getCluePict :: Clue -> Picture
getCluePict c
    | c == One   = write groundColor "1"
    | c == Two   = write groundColor "2"
    | c == Three = write groundColor "3"
    | c == Four  = write groundColor "4"
    | c == Five  = write groundColor "5"
    | c == Six   = write groundColor "6"
    | c == Seven = write groundColor "7"
    | c == Eight = write groundColor "8"
    | c == Mine  = write red "X"
    | otherwise  = Blank
    where write c t = translate (-cellSize/4) (-cellSize/4) $ text c 20 1 t

-- Cell Pictures ----------------------------------------------------------------------------

groundPict :: Picture
groundPict = pictures [
      color (dim groundColor) $ rectangleSolid cellSize cellSize
    , color groundColor $ rectangleSolid (cellSize - 5) (cellSize - 5)
    ]

dugPict :: Picture
dugPict = pictures [
      color (dim dugColor) $ rectangleSolid cellSize cellSize
    , color dugColor $ rectangleSolid (cellSize - 5) (cellSize - 5)
    ]

flagPict :: Picture
flagPict = pictures [
      groundPict
    , color red $
      polygon [ (- (cellSize/5), (cellSize/5)*2)
              , (- (cellSize/5), - (cellSize/5))
              , ((cellSize/5) * 2, 1.5)]
    , translate (- cellSize/5) 0.0
    $ color (dark red) $ rectangleSolid (cellSize/10) (cellSize - 5.0)
    ]

unsurePict :: Picture
unsurePict = pictures [
      groundPict
    , translate (-cellSize/4) (-cellSize/4) $ text white 20 1 "?"
    ]

snapToGrid :: Picture -> (Int, Int) -> Picture
snapToGrid p (column, row) = translate x y p
    where x = fromIntegral column * cellSize
          y = fromIntegral row    * cellSize

-- Menu Rendering ----------------------------------------------------------------------------

renderMenu :: State -> Picture
renderMenu state = pictures
    [ translate 0.0 vPos $
    pictures
        [ renderMainContainer
        , translate 0.0 (y/3) $
          pictures
            [ color (dim groundColor) $ rectangleSolid (x-20) (y / 3 - 20)
            , color groundColor $ rectangleSolid (x-30) (y / 3 - 30)
            , translate (hPos m1) (-10.0) $ text dugColor 20 1 m1
            ]
        , pictures
            [ color (dim groundColor) $ rectangleSolid (x-20) (y / 3 - 20)
            , color groundColor $ rectangleSolid (x-30) (y / 3 - 30)
            , translate (hPos m2) (-10.0) $ text dugColor 20 1 m2
            ]
        , translate 0.0 (-y/3) $
          pictures
            [ color (dim groundColor) $ rectangleSolid (x-20) (y / 3 - 20)
            , color groundColor $ rectangleSolid (x-30) (y / 3 - 30)
            , translate (hPos m3) (-10.0) $ text dugColor 20 1 m3 ]
        ]
    , header
    ]
    where   x        = fromIntegral $ fst smallGridSize
            y        = fromIntegral $ snd smallGridSize
            m1       = if state == Menu then "New Game" else "Small"
            m2       = if state == Menu then "Resume"   else "Normal"
            m3       = if state == Menu then "Exit"     else "Large"
            header   = renderHeader (show state)
            hPos str = - ( (fromIntegral (length str) * 16.5)/2)
            vPos     = - (fromIntegral smallWindowHeight/2 - y/2 - 20.0)

renderHeader :: String -> Picture
renderHeader title = translate 0.0 vPos $
               pictures [ color (bright boxBackground) $ rectangleSolid (x + 20) (y + 20)
                        , color (dim boxBackground) $ rectangleSolid (x + 6) (y + 6)
                        , color boxBackground $ rectangleSolid x y
                        , translate hPos 0.0 $ text dugColor 20 1 title ]
    where   x    = fromIntegral $ fst headerSize
            y    = fromIntegral $ snd headerSize
            hPos = - (fromIntegral (length title * 16)/2)
            vPos = fromIntegral smallWindowHeight/2 - y/2 - 20.0

-- Helper functions --------------------------------------------------------------------------

text :: Color -> Float -> Float -> String -> Picture
text col size boldness str =
    color col $
    pictures [ translate x y $ scale (size/100) (size/100) $ G.text str
             | x <- [(-boldness),(0.5-boldness)..boldness]
             , y <- [(-boldness),(0.5-boldness)..boldness]
             ]