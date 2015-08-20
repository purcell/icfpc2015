module Types where

import           Data.List (intercalate)
import           Data.Set  (Set)
import qualified Data.Set  as S


------------------------------------------------------------------------------
-- Problem definitions
------------------------------------------------------------------------------

type UnitPosition = Set Cell

data Unit = Unit { unitMembers :: UnitPosition
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

movementCommands :: [Command]
movementCommands = [Move E, Move W, Move SE, Move SW]

allCommands :: [Command]
allCommands = movementCommands ++ [Turn Clockwise, Turn AntiClockwise]

data Board = Board { boardWidth  :: Int
                   , boardHeight :: Int
                   , boardFilled :: Set Cell
                   }

boardXs :: Board -> [Int]
boardXs board = [0..(boardWidth board - 1)]
boardYs :: Board -> [Int]
boardYs board = [0..(boardHeight board - 1)]

isOccupied :: Board -> Cell -> Bool
isOccupied board c = c `S.member` boardFilled board

isEmptyPosition :: Board -> Cell -> Bool
isEmptyPosition board c@(Cell x y) = 0 <= x && x < boardWidth board &&
                                     0 <= y && y < boardHeight board &&
                                     not (isOccupied board c)

instance Show Board where
  show board = intercalate "\n" $ map showRow (boardYs board)
    where showRow y = (if odd y then " " else "") ++ unwords [showCell (Cell x y) | x <- boardXs board]
          showCell c = if isOccupied board c then "⬢" else "⬡"

-- The game state an AI sees
data GameState = GameState { gsGameOver             :: Bool
                           , gsUnitsPlaced          :: Int
                           , gsCurrentUnit          :: Maybe Unit
                           , gsCurrentUnitHistory   :: Set UnitPosition
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

