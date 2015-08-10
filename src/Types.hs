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
                 } deriving (Show, Eq)

instance Ord Cell where
  compare c1 c2 = compare (cellY c1, cellX c1) (cellY c2, cellX c2)


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
                   , boardFilled :: [Cell]
                   }

boardXs :: Board -> [Int]
boardXs board = [0..(boardWidth board - 1)]
boardYs :: Board -> [Int]
boardYs board = [0..(boardHeight board - 1)]

isOccupied :: Board -> Int -> Int -> Bool
isOccupied board x y = Cell x y `elem` boardFilled board

isEmptyPosition :: Board -> Int -> Int -> Bool  -- TODO: just use Cell, not Int -> Int
isEmptyPosition board x y = isOnBoard board (Cell x y) &&
                            not (isOccupied board x y)

isOnBoard :: Board -> Cell -> Bool
isOnBoard board (Cell x y) = 0 <= x && x < boardWidth board &&
                             0 <= y && y < boardHeight board

surroundingCells :: Cell -> [Cell]
surroundingCells (Cell x y) = aboveAndBelow ++ [Cell (x-1) y, Cell (x+1) y]
  where
    offset = if odd y then 0 else -1
    aboveAndBelow = [Cell (x' + offset) y' | x' <- [x, x + 1], y' <- [y - 1, y + 1]]

instance Show Board where
  show board = intercalate "\n" $ map showRow (boardYs board)
    where showRow y = (if odd y then " " else "") ++ unwords [showCell x y | x <- boardXs board]
          showCell x y = if isOccupied board x y then "⬢" else "⬡"

-- The game state an AI sees
data GameState = GameState { gsGameOver             :: Bool
                           , gsUnitsPlaced          :: Int
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

