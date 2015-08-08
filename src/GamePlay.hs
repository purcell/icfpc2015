module GamePlay
       --(createGame)
       where

import           Data.List     (intercalate)
import           Data.Maybe    (listToMaybe, mapMaybe)
import           Random        (getContestGen)
import           Rotation      (rotateAntiClockwiseAround,
                                rotateClockwiseAround)
import           System.Random (randoms)
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


data GameState = GameState { gsCurrentUnit          :: Maybe Unit
                           , gsBoard                :: Board
                           , gsScore                :: Int
                           , gsLinesClearedLastMove :: Int
                           , gsCommandHistory       :: [Command]
                           , gsUpcomingUnits        :: [Unit]
                           } deriving Show


-- TODO: prevent repeating previously-seen positions
-- An AI can use this function to see what effect its command will have
playCommand :: GameState -> Command -> GameState
playCommand gs _   | gameOver gs = gs
playCommand gs cmd =
  if isValidUnitPosition (gsBoard gs) movedUnit then
    gs { gsCurrentUnit = Just movedUnit
       , gsCommandHistory = updatedCommands
       }
  else
    gs { gsCurrentUnit = listToMaybe (gsUpcomingUnits gs)
       , gsUpcomingUnits = tail (gsUpcomingUnits gs)
       , gsCommandHistory = updatedCommands
       , gsBoard = newBoard
       , gsScore = newScore
       , gsLinesClearedLastMove = linesCleared
       }
  where
    Just currentUnit = gsCurrentUnit gs
    movedUnit = applyRawCommand cmd currentUnit
    updatedCommands = gsCommandHistory gs ++ [cmd]
    (linesCleared, newBoard) = clearLines $ addUnitCellsToBoard (gsBoard gs) currentUnit
    newScore = gsScore gs + moveScore
      where moveScore = points + lineBonus
            points = size + 100 * (1 + linesCleared) * linesCleared `div` 2
            lineBonus = if gsLinesClearedLastMove gs > 1
                        then (gsLinesClearedLastMove gs - 1) * points `div` 10
                        else 0
            size = length (unitMembers currentUnit)


gameOver :: GameState -> Bool
gameOver gs = case gsCurrentUnit gs of
  Nothing -> True
  Just unit -> not (isValidUnitPosition (gsBoard gs) unit)

-- playGame :: GameState -> (GameState -> Command) -> GameState
-- playGame gs strategy = steps
--   where steps = takeWhile (not gameOver) $ iterate

makeGameState :: Problem -> Int -> GameState
makeGameState problem seed = GameState (listToMaybe units) board 0 0 [] (tail units)
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

data MoveDirection = E | W | SE | SW
                   deriving Show
data TurnDirection = Clockwise | AntiClockwise
                   deriving Show

data Command = Move MoveDirection
             | Turn TurnDirection
             deriving Show

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


createGame :: Problem -> Game
createGame p = Game (createBoard p)


