module GamePlay
       --(createGame)
       where

import qualified Data.Foldable as F
import           Data.Maybe    (mapMaybe)
import qualified Data.Set      as S
import           Prelude       hiding (maximum, minimum)
import           Random        (getContestGen, randomInts)
import           Rotation      (rotateAntiClockwiseAround,
                                rotateClockwiseAround)
import           Types

------------------------------------------------------------------------------
-- Units
------------------------------------------------------------------------------

spawnUnit :: Board -> Unit -> Unit
spawnUnit board (Unit cells pivot) = Unit { unitMembers = S.map offsetCell cells
                                          , unitPivot = offsetCell pivot
                                          }
  where offsetCell = applyOffsets initialOffsetX initialOffsetY
        initialOffsetY = negate unitMinY
        initialOffsetX = (boardWidth board - (1 + unitMaxX - unitMinX)) `div` 2 - unitMinX
        unitMinY = F.minimum $ S.map cellY cells
        unitMinX = F.minimum $ S.map cellX cells
        unitMaxX = F.maximum $ S.map cellX cells


applyOffsets :: Int -> Int -> Cell -> Cell
applyOffsets x y (Cell cx cy) = Cell (x + cx) (y + cy)

isValidUnitPosition :: Board -> Unit -> Bool
isValidUnitPosition board unit = all (isEmptyPosition board) (S.toList (unitMembers unit))

unitRotateClockwise :: Unit -> Unit
unitRotateClockwise (Unit cells pivot) = Unit (rotateClockwiseAround pivot cells) pivot

unitRotateAntiClockwise :: Unit -> Unit
unitRotateAntiClockwise (Unit cells pivot) = Unit (rotateAntiClockwiseAround pivot cells) pivot

unitTranslate :: (Cell -> Cell) -> Unit -> Unit
unitTranslate f (Unit cells pivot) = Unit (S.map f cells) (f pivot)



------------------------------------------------------------------------------
-- Updating the board and tracking scores
------------------------------------------------------------------------------

addUnitCellsToBoard :: Board -> Unit -> Board
addUnitCellsToBoard board unit = board { boardFilled = boardFilled board `S.union` unitMembers unit }


clearLines :: Board -> (Int, Board)
clearLines board = (numFullLines, board { boardFilled = updatedBoardCells })
  where
    toClear = rowsToClear board
    numFullLines = length toClear
    updatedBoardCells = foldl removeRow (boardFilled board) toClear
    removeRow oldCells row = S.fromList $ mapMaybe (removeOrMoveCell row) $ S.toList oldCells
    removeOrMoveCell :: Int -> Cell -> Maybe Cell
    removeOrMoveCell row (Cell _ y) | y == row = Nothing
    removeOrMoveCell row (Cell x y) | y < row  = Just (Cell x (y + 1))
    removeOrMoveCell _    cell                 = Just cell


rowsToClear :: Board -> [Int]
rowsToClear board = filter fullRow [0..(boardHeight board - 1)]
  where fullRow y = and [isOccupied board (Cell x y) | x <- boardXs board]


-- An AI can use this function to see what effect its command will have
playCommand :: GameState -> Command -> (CommandResult, GameState)
playCommand gs _   | gsGameOver gs = (IllegalCommand, gs)
playCommand gs cmd =
  if isValidUnitPosition (gsBoard gs) proposedUnit then
    if proposedUnitPosition `S.member` gsCurrentUnitHistory gs then
      (IllegalCommand, endGame (gs { gsScore = 0 }))
    else
      (UnitMoved,
       (moveToProposedPosition . saveCommand) gs)
  else
    (UnitLocked,
     (switchToNextUnit . lockAndScoreCurrentUnit . saveCommand) gs)
  where
    Just currentUnit = gsCurrentUnit gs
    proposedUnit = applyRawCommand cmd currentUnit
    proposedUnitPosition = unitMembers proposedUnit
    moveToProposedPosition s = s { gsCurrentUnit = Just proposedUnit
                                 , gsCurrentUnitHistory = S.insert proposedUnitPosition (gsCurrentUnitHistory s)
                                 , gsLinesClearedLastMove = 0
                                 }
    saveCommand s = s { gsCommandHistory = gsCommandHistory gs ++ [cmd] }


endGame :: GameState -> GameState
endGame gs = gs { gsGameOver = True }

lockAndScoreCurrentUnit :: GameState -> GameState
lockAndScoreCurrentUnit gs = gs { gsBoard = newBoard
                                , gsScore = moveScore + gsScore gs
                                , gsLinesClearedLastMove = linesCleared
                                , gsUnitsPlaced = gsUnitsPlaced gs + 1
                                }
  where
    Just currentUnit = gsCurrentUnit gs
    (linesCleared, newBoard) = clearLines $ addUnitCellsToBoard (gsBoard gs) currentUnit
    moveScore = points + lineBonus
      where
        points = size + ((100 * (1 + linesCleared) * linesCleared) `div` 2)
        lineBonus = if gsLinesClearedLastMove gs > 1
                    then ((gsLinesClearedLastMove gs - 1) * points) `div` 10
                    else 0
        size = S.size (unitMembers currentUnit)


switchToNextUnit :: GameState -> GameState
switchToNextUnit gs = case gsUpcomingUnits gs of
  (u:us) | validPosition u -> gs { gsCurrentUnit = Just u
                                 , gsUpcomingUnits = us
                                 , gsCurrentUnitHistory = S.singleton (unitMembers u) }
  _                        -> endGame gs
  where validPosition = isValidUnitPosition (gsBoard gs)

makeGameState :: Problem -> Int -> GameState
makeGameState problem seed = switchToNextUnit
                             GameState { gsGameOver = False
                                       , gsCurrentUnit = Nothing
                                       , gsUnitsPlaced = 0
                                       , gsCurrentUnitHistory = S.empty
                                       , gsBoard = board
                                       , gsScore = 0
                                       , gsLinesClearedLastMove = 0
                                       , gsCommandHistory = []
                                       , gsUpcomingUnits = units
                                       }
  where
    board = createBoard problem
    randomUnitIndices = map (`mod` (length $ problemUnits problem)) $ randomInts (getContestGen seed)
    randomUnits = map (problemUnits problem !!) randomUnitIndices
    units = map (spawnUnit board) $ take (problemSourceLength problem) randomUnits

createBoard :: Problem -> Board
createBoard p = Board (problemWidth p) (problemHeight p) (S.fromList (problemFilled p))

gameBoardWithCurrentUnit :: GameState -> Board
gameBoardWithCurrentUnit gs =
  case gsCurrentUnit gs of
  Nothing -> gsBoard gs
  Just unit -> addUnitCellsToBoard (gsBoard gs) unit

------------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------------

commandChar :: Command -> Char
commandChar (Move E)             = 'e' -- {b, c, e, f, y, 2}
commandChar (Move W)             = '!' -- {p, ', !, ., 0, 3}
commandChar (Move SE)            = 'l' -- {l, m, n, o, space, 5}
commandChar (Move SW)            = 'i' -- {a, g, h, i, j, 4}
commandChar (Turn Clockwise)     = 'd' -- {d, q, r, v, z, 1}
commandChar (Turn AntiClockwise) = 'k' -- {k, s, t, u, w, x}


moveCell :: MoveDirection -> Cell -> Cell
moveCell E (Cell x y) = Cell (x + 1) y
moveCell W (Cell x y) = Cell (x - 1) y
moveCell SW (Cell x y) = Cell (if odd y then x else x - 1) (y + 1)
moveCell SE (Cell x y) = Cell (if even y then x else x + 1) (y + 1)


applyRawCommand :: Command -> Unit -> Unit
applyRawCommand (Move direction) = unitTranslate (moveCell direction)
applyRawCommand (Turn Clockwise) = unitRotateClockwise
applyRawCommand (Turn AntiClockwise) = unitRotateAntiClockwise



