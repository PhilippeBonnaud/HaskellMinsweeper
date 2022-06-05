module Game where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Game datatype ----------------------------------------------------------------------------

data Game      = Game {
    state      :: State          -- Is the game running ?
  , minefield  :: Minefield      -- Description of the game grid
  , minesLeft  :: Int            -- Number of mines yet to be found
  , mineList   :: [Coord]        -- Infinite list of random mmines
  , timerFlash :: Int            -- Timer to be shown
  , tickPeriod :: Float          -- Time between game updates
  , levelDesc  :: Level          --
  , playing    :: (Maybe (Int, Int), Maybe Button) -- last player move
  } deriving (Eq, Show)
data State     = Running | GameOver | Menu | LevelSelect
  deriving (Eq, Show)
data Button    = RightB | LeftB deriving (Eq, Show)
type Minefield = Array Coord (Cell, Clue)

-- Cell datatypes ---------------------------------------------------------------------------

type Coord     = (Int, Int)
data Clue      = One | Two | Three | Four | Five | Six | Seven | Eight | Mine | Empty
  deriving (Eq, Show)
data Cell      = Ground | Flag | Unsure | Dug
  deriving (Eq, Show)

-- Level datatype ---------------------------------------------------------------------------

data Level     = Level {
    level      :: LevelDesc
  , x          :: Int
  , y          :: Int
  , mines      :: Int
  } deriving (Eq, Show)

data LevelDesc = Small | Normal | Big
  deriving (Eq, Show)

-- Initializers -----------------------------------------------------------------------------

initApp :: Game
initApp = Game {
    state      = Menu
  , minefield  = initMinefield (0,0)
  , minesLeft  = 0
  , mineList   = []
  , timerFlash = 0
  , tickPeriod = 0.0
  , levelDesc  = initLevel
  , playing    = (Nothing, Nothing)
  }

initGame :: StdGen -> StdGen -> Game
initGame g h   = Game {
    state      = Running
  , minefield  = initMinefield gridSize
  , minesLeft  = minesNb
  , mineList   = initMines g h gridSize
  , timerFlash = time
  , tickPeriod = 0.0
  , levelDesc  = initLevel
  , playing    = (Nothing, Nothing)
  } where time     = 0
          lvl      = initLevel
          minesNb  = mines lvl
          gridSize = (x lvl, y lvl)

initLevel :: Level
initLevel = Level {
    level     = Small
  , x         = 8
  , y         = 8
  , mines     = 10
  }

initMines :: StdGen -> StdGen -> (Int, Int) -> [Coord]
initMines g h (x, y) = zip (randomRs (0, x - 1) g :: [Int]) (randomRs (0, y - 1) h :: [Int])

initMinefield :: (Int, Int) -> Minefield
initMinefield (x,y) = array indexRange $ zip (range indexRange) (repeat (Ground, Empty))
  where indexRange = ((0,0), (x - 1, y - 1))