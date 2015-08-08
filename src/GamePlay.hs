module GamePlay
       --(createGame)
       where

import           Data.List  (intercalate)
import           Data.Maybe (mapMaybe)
import           Rotation   (rotateAntiClockwiseAround, rotateClockwiseAround)
import           Types

------------------------------------------------------------------------------
-- Board inspection and manipulation
------------------------------------------------------------------------------


data Board = Board { boardWidth  :: Int
                   , boardHeight :: Int
                   , boardFilled :: [Cell]
                   }

instance Show Board where
  show board = intercalate "\n" $ map showRow (boardYs board)
    where showRow y = (if odd y then " " else "") ++ unwords [showCell x y | x <- boardXs board]
          showCell x y = if isOccupied board x y then "⬢" else "⬡"


boardXs :: Board -> [Int]
boardXs board = [0..(boardWidth board - 1)]
boardYs :: Board -> [Int]
boardYs board = [0..(boardHeight board - 1)]

isOccupied :: Board -> Int -> Int -> Bool
isOccupied board x y = Cell x y `elem` boardFilled board

isValidPosition :: Board -> Int -> Int -> Bool
isValidPosition board x y = 0 <= x && x < (boardWidth board) &&
                            0 <= y && y < (boardHeight board) &&
                            not (isOccupied board x y)

------------------------------------------------------------------------------
-- Overall game state
------------------------------------------------------------------------------

data Game = Game { gameBoard :: Board }

instance Show Game where
  show = show . gameBoard



------------------------------------------------------------------------------
-- Units
------------------------------------------------------------------------------

spawnUnit :: Board -> Unit -> Unit
spawnUnit board (Unit cells pivot) = Unit { unitMembers = map offsetCell cells
                                          , unitPivot = offsetCell pivot
                                          }
  where offsetCell = applyOffsets initialOffsetX initialOffsetY
        initialOffsetY = negate unitMinY
        initialOffsetX = (boardWidth board - (1 + unitMaxX - unitMinX)) `div` 2 - unitMinX
        unitMinY = minimum $ map cellY cells
        unitMinX = minimum unitCellXs
        unitMaxX = maximum unitCellXs
        unitCellXs = map cellX cells


applyOffsets :: Int -> Int -> Cell -> Cell
applyOffsets x y (Cell cx cy) = Cell (x + cx) (y + cy)




isValidUnitPosition :: Board -> Unit -> Bool
isValidUnitPosition board unit = all (\(Cell x y) -> isValidPosition board x y) (unitMembers unit)

unitRotateClockwise :: Unit -> Unit
unitRotateClockwise (Unit cells pivot) = Unit (rotateClockwiseAround pivot cells) pivot

unitRotateAntiClockwise :: Unit -> Unit
unitRotateAntiClockwise (Unit cells pivot) = Unit (rotateAntiClockwiseAround pivot cells) pivot

unitTranslate :: (Cell -> Cell) -> Unit -> Unit
unitTranslate f (Unit cells pivot) = Unit (map f cells) (f pivot)



------------------------------------------------------------------------------
-- Updating the board and tracking scores
------------------------------------------------------------------------------

data ScoringFactors = ScoringFactors { sfUnit :: Unit, sfLinesCleared :: Int }

addUnitCellsToBoard :: Board -> Unit -> Board
addUnitCellsToBoard board unit = board { boardFilled = boardFilled board ++ unitMembers unit }



lockUnit :: Board -> Unit -> Board
lockUnit board unit = newBoard
  where (linesCleared, newBoard) = clearLines $ addUnitCellsToBoard board unit


clearLines :: Board -> (Int, Board)
clearLines board = (numFullLines, board { boardFilled = movedCells numFullLines })
  where
    numFullLines = linesToClear board
    movedCells n = mapMaybe (moveCellDown n) (boardFilled board)
    moveCellDown n (Cell x y) = if y' > (boardHeight board - 1) then Nothing
                                else Just (Cell x y')
      where y' = y + n


linesToClear :: Board -> Int
linesToClear board = length $ takeWhile id [fullRow y | y <- reverse [0..(boardHeight board - 1)]]
  where fullRow y = and [isOccupied board x y | x <- boardXs board]


data GameState = GameState { gsBoard :: Board
                           , gsScore :: Int
                           , gsLinesClearedLastMove
                           , gsCommandHistory :: [Command]
                           , gsUpcomingUnits :: [Unit]
                           }



------------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------------

data MoveDirection = E | W | SE | SW
data TurnDirection = Clockwise | AntiClockwise

data Command = Move MoveDirection
             | Turn TurnDirection

commandChar :: Command -> Char
commandChar (Move E) = 'b'
commandChar (Move W) = 'p'
commandChar (Move SE) = 'l'
commandChar (Move SW) = 'a'
commandChar (Turn Clockwise) = 'd'
commandChar (Turn AntiClockwise) = 'k'


moveCell :: MoveDirection -> Cell -> Cell
moveCell E (Cell x y) = Cell (x + 1) y
moveCell W (Cell x y) = Cell (x - 1) y
moveCell SE (Cell x y) = Cell (if odd y then x else x - 1) (y + 1)
moveCell SW (Cell x y) = Cell (if even y then x else x + 1) (y + 1)


applyRawCommand :: Command -> Unit -> Unit
applyRawCommand (Move direction) = unitTranslate (moveCell direction)
applyRawCommand (Turn Clockwise) = unitRotateClockwise
applyRawCommand (Turn AntiClockwise) = unitRotateAntiClockwise



------------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------------

createBoard :: Problem -> Board
createBoard p = Board (problemWidth p) (problemHeight p) (problemFilled p)


createGame :: Problem -> Game
createGame p = Game (createBoard p)




