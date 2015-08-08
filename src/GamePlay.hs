module GamePlay (createGame) where

import           Data.List (intercalate)
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


data PlacedUnit = PlacedUnit { puUnit     :: Unit
                             , puOffsetX  :: Int
                             , puOffsetY  :: Int
                             , puRotation :: Int } deriving Show

placedUnitCells :: PlacedUnit -> [Cell]
placedUnitCells pUnit | puRotation pUnit == 0 = map applyOffset $ unitMembers $ puUnit pUnit
  where applyOffset (Cell x y) = Cell { cellX = x + puOffsetX pUnit, cellY = y + puOffsetY pUnit}
placedUnitCells _ = error "rotation not yet handled"

spawnUnit :: Board -> Unit -> PlacedUnit
spawnUnit board unit = PlacedUnit { puUnit = unit
                                  , puOffsetX = initialOffsetX
                                  , puOffsetY = initialOffsetY
                                  , puRotation = 0 }
  where initialOffsetY = negate unitMinY
        initialOffsetX = (boardWidth board - (1 + unitMaxX - unitMinX)) `div` 2 - unitMinX
        unitMinY = minimum $ map cellY originalCells
        unitMinX = minimum unitCellXs
        unitMaxX = maximum unitCellXs
        originalCells = unitMembers unit
        unitCellXs = map cellX originalCells

lockUnit :: Board -> PlacedUnit -> Board
lockUnit board placedUnit = board { boardFilled = (boardFilled board) ++ placedUnitCells placedUnit }


isValidUnitPosition :: Board -> PlacedUnit -> Bool
isValidUnitPosition board plUnit = all (\(Cell x y) -> isValidPosition board x y) (placedUnitCells plUnit)


------------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------------

createBoard :: Problem -> Board
createBoard p = Board (problemWidth p) (problemHeight p) (problemFilled p)


createGame :: Problem -> Game
createGame p = Game (createBoard p)




