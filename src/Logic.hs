{-#LANGUAGE TupleSections#-}

module Logic where

import Data.Array
import Data.List (group, sort, nub)
import Data.Maybe
import Game
import Render
import System.Random (StdGen, newStdGen)

update :: Float -> Game -> Game
update t game
    | s == Running     = tickTime t . checkPlay . revealBoard . checkGameOver . checkFirst $ game
    | s == GameOver    = checkGameOverInput game
    | s == Menu        = checkMenuInput game
    | s == LevelSelect = checkMenuInput game
    | otherwise        = game
    where s = state game

-- First move -- Minefield population ------------------------------------------

checkFirst :: Game -> Game
checkFirst game
    | isInitialMinefield = populateMinefield game
    | otherwise          = game
    where isInitialMinefield  = null mfFiltered && isPlaying
          mf                  = minefield game
          mfFiltered          = filter (\(_, (c,_)) -> c /= Ground) $ assocs mf
          isPlaying           = isJust $ fst $ playing game

populateMinefield :: Game -> Game
populateMinefield game = generateClues size $ dropMines nbM played game mineLs
    where mineLs = mineList  game
          level  = levelDesc game
          nbM    = mines level
          size   = (x level, y level)
          played = fromJust . fst $ playing game

dropMines :: Int -> Coord -> Game -> [Coord] -> Game
dropMines n c g ms = g { minefield = mf // map (, (Ground, Mine)) validMs}
    where validMs = take n $ nub $ filter (`notElem` safeZone c) ms
          mf      = minefield g

generateClues :: (Int, Int) -> Game -> Game
generateClues (x, y) g = g { minefield = mf // selectedClues}
    where selectedClues = filter (\(coord, _)   -> coord `notElem` minePosition)
                        $ filter (\(coord, _) -> inMinefield coord)
                        $ map (\xs -> (head xs, (Ground, intToClue $ length xs)))
                        $ group $ sort $ concat
                        $ foldr (\x c -> flip (:) c $ radius g . fst $ x) []
                        $ filter (\(_, (_, cl)) -> cl == Mine)
                        $ assocs mf
          mf            = minefield g
          minePosition  = map fst
                        $ filter (\(_, (_, cl)) -> cl == Mine)
                        $ assocs mf
          inMinefield (c,r) = c < x && r < y && c >= 0 && r >= 0

radius :: Game -> Coord -> [Coord]
radius g (c, r)     = filter inMinefield ring
    where ls        = [(x,y)| x <- [-1..1], y <- [-1..1], abs x + abs y /= 0]
          ring      = zip (map ((+ c) . fst) ls) (map ((+ r) . snd) ls)
          x'        = x (levelDesc g)
          y'        = y (levelDesc g)
          inMinefield (c, r) = c >= 0 && r >= 0 && c < x' && r < y'

safeZone :: Coord -> [Coord]
safeZone (x, y) = zip (map ((+ x) . fst) ls) (map ((+ y) . snd) ls)
    where ls = [ (x,y) | x <- [-2..2], y <- [-2..2], abs x + abs y < 3]

intToClue :: Int -> Clue
intToClue i
    | i == 1    = One
    | i == 2    = Two
    | i == 3    = Three
    | i == 4    = Four
    | i == 5    = Five
    | i == 6    = Six
    | i == 7    = Seven
    | i == 8    = Eight
    | otherwise = Empty

-- Menu -------------------------------------------------------------------------

checkMenuInput :: Game -> Game
checkMenuInput = undefined

-- GameOver ---------------------------------------------------------------------

checkGameOver :: Game -> Game
checkGameOver g = if revealedMine
                  then g
                  else g { state = GameOver }
    where mineList     = filter (\(_, (ce, cl)) -> ce == Dug && cl == Mine) mf
          mf           = assocs $ minefield g
          revealedMine = null mineList

checkGameOverInput :: Game -> Game
checkGameOverInput game
--  | isJust input = gameEnded game
    | otherwise    = game
    where input = snd $ playing game

{-
    gameEnded :: Game -> Game
    gameEnded game
        | button == LeftB  = main
        | button == RightB = game { state = Menu
                                , playing = (Nothing, Nothing)
                                }
        | otherwise = game { playing = (Nothing, Nothing)}
        where button = fromJust $ snd $ playing game
-}

-- Playing ----------------------------------------------------------------------

checkPlay :: Game -> Game
checkPlay game = if isNothing playedCell
                 then game
                 else playGame game
    where playedCell = fst $ playing game

playGame :: Game -> Game
playGame game
    | playStatus == Just RightB = game { minefield = mark game playedCell
                                       , playing = (Nothing, Nothing) }
    | playStatus == Just LeftB  = game { minefield = dig game playedCell
                                       , playing = (Nothing, Nothing) }
    | otherwise                 = game
    where playStatus = snd $ playing game
          playedCell = fromJust $ fst $ playing game

dig :: Game -> (Int, Int) -> Minefield
dig game playedCell
    | cell == Ground = mf // [(playedCell, (Dug, clue))]
    | otherwise      = mf
    where (cell, clue) = mf ! playedCell
          mf           = minefield game

revealBoard :: Game -> Game
revealBoard g
    | st == Running     = g { minefield = mf // toBeDug }
    | st == GameOver    = g { minefield = mf // revealMines }
    where revealedEmpty = findEmptyDug mf
          revealMines   = findMines mf
          toBeDug       = concatMap (diggingDeeper g) revealedEmpty
          mf            = minefield g
          st            = state g

findEmptyDug :: Minefield -> [Coord]
findEmptyDug mf = map fst
    $ filter (\(_, (cell, clue)) -> cell == Dug && clue == Empty)
    $ assocs mf

findMines :: Minefield -> [(Coord, (Cell, Clue))]
findMines mf = map (\(co, (_, cl)) -> (co, (Dug, cl)))
    $ filter (\(_, (_, clue)) -> clue == Mine)
    $ assocs mf

diggingDeeper :: Game -> Coord -> [(Coord, (Cell, Clue))]
diggingDeeper g (x, y) = map (
                                (\(c, (ce, cl)) -> (c, (Dug, cl)))
                              . (\ c -> (c, mf ! c))
                              )
                        $ radius g (x, y)
    where mf = minefield g

mark :: Game -> (Int, Int) -> Minefield
mark game playedCell
    | cell == Ground = mf // [(playedCell, (Flag,   clue))]
    | cell == Flag   = mf // [(playedCell, (Unsure, clue))]
    | cell == Unsure = mf // [(playedCell, (Ground, clue))]
    | otherwise      = mf
    where (cell, clue) = mf ! playedCell
          mf           = minefield game

-- Timer ------------------------------------------------------------------------

tickTime :: Float -> Game -> Game
tickTime t g = g { timerFlash = floor (tick + t)
                 , tickPeriod = tick + t
                 }
    where tick = tickPeriod g

-- Input Handlers ---------------------------------------------------------------

mousePositionAsCellPos :: (Float, Float) -> Maybe (Int, Int)
mousePositionAsCellPos (x, y) =
    if mousePosInRange g w h (x, y)
    then Just ( floor ((x + (w/2 - 20.0)) / cellSize)
              , floor (abs (y + (h/2 - 20.0))/ cellSize)
              )
    else Nothing
    where g = fromIntegral $ snd smallGridSize
          w = fromIntegral smallWindowWidth
          h = fromIntegral smallWindowHeight

mousePosInRange :: Float -> Float -> Float -> (Float, Float) -> Bool
mousePosInRange g w h (x, y)
    | x < - (w/2 - 20.0)               = False
    | x > w/2 - 21.0                   = False
    | y > g/2 - (h/2 - g/2 - 20.0) - 1 = False
    | y < - (h/2 - 20.0)               = False
    | otherwise                        = True
