module GamePlay
       --(createGame)
       where

import           Data.Function (on)
import           Data.List     (sort)
import           Data.Maybe    (listToMaybe, mapMaybe)
import           Random        (getContestGen)
import           Rotation      (rotateAntiClockwiseAround,
                                rotateClockwiseAround)
import           System.Random (randoms)
import           Types

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
isValidUnitPosition board unit = all (\(Cell x y) -> isEmptyPosition board x y) (unitMembers unit)

unitRotateClockwise :: Unit -> Unit
unitRotateClockwise (Unit cells pivot) = Unit (rotateClockwiseAround pivot cells) pivot

unitRotateAntiClockwise :: Unit -> Unit
unitRotateAntiClockwise (Unit cells pivot) = Unit (rotateAntiClockwiseAround pivot cells) pivot

unitTranslate :: (Cell -> Cell) -> Unit -> Unit
unitTranslate f (Unit cells pivot) = Unit (map f cells) (f pivot)



------------------------------------------------------------------------------
-- Updating the board and tracking scores
------------------------------------------------------------------------------

addUnitCellsToBoard :: Board -> Unit -> Board
addUnitCellsToBoard board unit = board { boardFilled = boardFilled board ++ unitMembers unit }


clearLines :: Board -> (Int, Board)
clearLines board = (numFullLines, board { boardFilled = updatedBoardCells })
  where
    toClear = linesToClear board
    numFullLines = length toClear
    updatedBoardCells = foldl removeRow (boardFilled board) toClear
    removeRow oldCells row = mapMaybe (removeOrMoveCell row) oldCells
    removeOrMoveCell :: Int -> Cell -> Maybe Cell
    removeOrMoveCell row (Cell _ y) | y == row = Nothing
    removeOrMoveCell row (Cell x y) | y < row  = Just (Cell x (y + 1))
    removeOrMoveCell _    cell                 = Just cell


linesToClear :: Board -> [Int]
linesToClear board = filter fullRow [0..(boardHeight board - 1)]
  where fullRow y = and [isOccupied board x y | x <- boardXs board]


-- TODO: prevent repeating previously-seen positions
-- An AI can use this function to see what effect its command will have
playCommand :: GameState -> Command -> (CommandResult, GameState)
playCommand gs _   | gsGameOver gs = (IllegalCommand, gs)
playCommand gs cmd =
  if isValidUnitPosition (gsBoard gs) proposedUnit then
    if any (isSamePosition proposedUnit) previousPositions then
      (IllegalCommand, endGame gs)
    else
      (UnitMoved,
       (moveToProposedPosition . saveCommand) gs)
  else
    (UnitLocked,
     (switchToNextUnit . lockAndScoreCurrentUnit . saveCommand) gs)
  where
    Just currentUnit = gsCurrentUnit gs
    previousPositions =  currentUnit : gsCurrentUnitHistory gs
    proposedUnit = applyRawCommand cmd currentUnit
    moveToProposedPosition s = s { gsCurrentUnit = Just proposedUnit
                                 , gsCurrentUnitHistory = previousPositions
                                 }
    saveCommand s = s { gsCommandHistory = gsCommandHistory gs ++ [cmd] }



endGame :: GameState -> GameState
endGame gs = gs { gsGameOver = True }

lockAndScoreCurrentUnit :: GameState -> GameState
lockAndScoreCurrentUnit gs = gs { gsBoard = newBoard
                                , gsScore = newScore
                                , gsLinesClearedLastMove = linesCleared
                                }
  where
    Just currentUnit = gsCurrentUnit gs
    (linesCleared, newBoard) = clearLines $ addUnitCellsToBoard (gsBoard gs) currentUnit
    newScore = gsScore gs + moveScore
      where moveScore = points + lineBonus
            points = size + 100 * (1 + linesCleared) * linesCleared `div` 2
            lineBonus = if gsLinesClearedLastMove gs > 1
                        then (gsLinesClearedLastMove gs - 1) * points `div` 10
                        else 0
            size = length (unitMembers currentUnit)


switchToNextUnit :: GameState -> GameState
switchToNextUnit gs = case gsUpcomingUnits gs of
  []     -> endGame gs
  (u:us) -> gs { gsCurrentUnit = Just u
               , gsUpcomingUnits = us
               , gsCurrentUnitHistory = [] }

isSamePosition :: Unit -> Unit -> Bool
isSamePosition = (==) `on` (sort . unitMembers)

makeGameState :: Problem -> Int -> GameState
makeGameState problem seed = GameState { gsGameOver = False   -- Assumes problems are always valid
                                       , gsCurrentUnit = listToMaybe units
                                       , gsCurrentUnitHistory = []
                                       , gsBoard = board
                                       , gsScore = 0
                                       , gsLinesClearedLastMove = 0
                                       , gsCommandHistory = []
                                       , gsUpcomingUnits = tail units
                                       }
  where
    board = createBoard problem
    randomUnitIndices = map (`mod` (length $ problemUnits problem)) $ randoms (getContestGen seed)
    randomUnits = map (problemUnits problem !!) randomUnitIndices
    units = map (spawnUnit board) $ take (problemSourceLength problem) randomUnits


gameBoardWithCurrentUnit :: GameState -> Board
gameBoardWithCurrentUnit gs =
  case gsCurrentUnit gs of
  Nothing -> gsBoard gs
  Just unit -> addUnitCellsToBoard (gsBoard gs) unit

------------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------------

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
moveCell SW (Cell x y) = Cell (if odd y then x else x - 1) (y + 1)
moveCell SE (Cell x y) = Cell (if even y then x else x + 1) (y + 1)


applyRawCommand :: Command -> Unit -> Unit
applyRawCommand (Move direction) = unitTranslate (moveCell direction)
applyRawCommand (Turn Clockwise) = unitRotateClockwise
applyRawCommand (Turn AntiClockwise) = unitRotateAntiClockwise



------------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------------

createBoard :: Problem -> Board
createBoard p = Board (problemWidth p) (problemHeight p) (problemFilled p)


runGame :: GameState -> Strategy -> GameState
runGame gs strategy = head $ filter gsGameOver steps
  where
    steps = iterate stepState gs
    stepState s = snd $ playCommand s (strategy s)


