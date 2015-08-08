module GamePlay
       --(createGame)
       where

import           Data.List (intercalate)
import           Rotation  (rotateAntiClockwiseAround, rotateClockwiseAround)
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

lockUnit :: Board -> Unit -> Board
lockUnit board unit = board { boardFilled = (boardFilled board) ++ unitMembers unit }

isValidUnitPosition :: Board -> Unit -> Bool
isValidUnitPosition board plUnit = all (\(Cell x y) -> isValidPosition board x y) (unitMembers plUnit)

unitRotateClockwise :: Unit -> Unit
unitRotateClockwise (Unit cells pivot)= Unit (rotateClockwiseAround pivot cells) pivot

unitRotateAntiClockwise :: Unit -> Unit
unitRotateAntiClockwise (Unit cells pivot)= Unit (rotateAntiClockwiseAround pivot cells) pivot


------------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------------

createBoard :: Problem -> Board
createBoard p = Board (problemWidth p) (problemHeight p) (problemFilled p)


createGame :: Problem -> Game
createGame p = Game (createBoard p)




