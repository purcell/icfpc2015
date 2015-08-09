module Types where

import           Data.List (intercalate)


------------------------------------------------------------------------------
-- Problem definitions
------------------------------------------------------------------------------

data Unit = Unit { unitMembers :: [Cell] -- TODO sorted bag
                 , unitPivot   :: Cell
                 } deriving Show

data Cell = Cell { cellX :: Int
                 , cellY :: Int
                 } deriving (Show, Eq, Ord)


data Problem = Problem { problemId           :: Int
                       , problemUnits        :: [Unit]
                       , problemWidth        :: Int
                       , problemHeight       :: Int
                       , problemFilled       :: [Cell]
                       , problemSourceLength :: Int
                       , problemSourceSeeds  :: [Int]
                       } deriving Show


------------------------------------------------------------------------------
-- Problem solutions
------------------------------------------------------------------------------

data Solution = Solution { solutionId       :: Int
                         , solutionSeed     :: Int
                         , solutionCommands :: String
                         } deriving Show

------------------------------------------------------------------------------
-- Gameplay
------------------------------------------------------------------------------

-- Commands an AI can use

data MoveDirection = E | W | SE | SW
                   deriving Show
data TurnDirection = Clockwise | AntiClockwise
                   deriving Show

data Command = Move MoveDirection
             | Turn TurnDirection
             deriving Show


data Board = Board { boardWidth  :: Int
                   , boardHeight :: Int
                   , boardFilled :: [Cell]
                   }

boardXs :: Board -> [Int]
boardXs board = [0..(boardWidth board - 1)]
boardYs :: Board -> [Int]
boardYs board = [0..(boardHeight board - 1)]

isOccupied :: Board -> Int -> Int -> Bool
isOccupied board x y = Cell x y `elem` boardFilled board

isEmptyPosition :: Board -> Int -> Int -> Bool
isEmptyPosition board x y = 0 <= x && x < boardWidth board &&
                            0 <= y && y < boardHeight board &&
                            not (isOccupied board x y)

instance Show Board where
  show board = intercalate "\n" $ map showRow (boardYs board)
    where showRow y = (if odd y then " " else "") ++ unwords [showCell x y | x <- boardXs board]
          showCell x y = if isOccupied board x y then "⬢" else "⬡"

-- The game state an AI sees
data GameState = GameState { gsGameOver             :: Bool
                           , gsCurrentUnit          :: Maybe Unit
                           , gsCurrentUnitHistory   :: [Unit]
                           , gsBoard                :: Board
                           , gsScore                :: Int
                           , gsLinesClearedLastMove :: Int
                           , gsCommandHistory       :: [Command]
                           , gsUpcomingUnits        :: [Unit]
                           } deriving Show


data CommandResult = UnitMoved
                   | IllegalCommand
                   | UnitLocked
                   deriving Show


-- What an AI must provide
type Strategy = GameState -> Command
