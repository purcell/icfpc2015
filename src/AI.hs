module AI where

import           Control.Exception        (assert)
import           Control.Monad.State.Lazy
import           Data.Foldable            (maximumBy)
import           Data.Function            (on)
import           Data.List                (sort, sortBy)
import           Data.Maybe               (fromJust)
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Tree
import           Debug.Trace
import           GamePlay
import           Types




-- Possible fitness criteria:
--   maximise turn score
--   minimise open cells around placed unit
--   maximise average y of placed unit
--   doesn't end the game
--   minimise entropy for the board
--   least empty cells below

turnFitness :: GameState -> GameState -> Float
turnFitness initial final = turnScore
                            + heavinessScore
                            -- - fromIntegral (numEmptyNeighbours finalBoard)
                            -- - averageEmptyNeighbours finalBoard
                            -- - entropyPenalty
                            -- - gameOverPenalty
  where
    finalBoard = gsBoard final
    turnScore = fromIntegral $ gsScore final - gsScore initial
    heavinessScore = averageCellY2 final
    -- entropyPenalty = average (cellEntropies finalBoard)
    --gameOverPenalty = if gsGameOver final then 1000
    --                  else 0

cellEntropies :: Board -> [Int]
cellEntropies b = map cellEntropy $ boardFilled b
  where
    cellEntropy c = sum $ map (neighbourEntropy c) (surroundingCells c)
    -- The more empty cells a piece has beside or below it, the higher its entropy
    -- Empty space below is much worse than to the sides
    -- Empty spaces low in the board are worse than higher up
    neighbourEntropy (Cell _ y) (Cell _ y') | y' < y && y' > 0          = 0
    neighbourEntropy _          c'          | not (isOnBoard b c')      = 0
    neighbourEntropy _          c'          | c' `S.member` filledCells = 0
    neighbourEntropy (Cell _ y) (Cell _ y') | y == y'                   = 1 + (y `div` 2)
    neighbourEntropy (Cell _ y) _                                       = 1 + y
    filledCells = S.fromList $ boardFilled b

numEmptyNeighbours :: Board -> Int
numEmptyNeighbours b = sum $ map numEmptyCellNeighbours $ boardFilled b
  where
    numEmptyCellNeighbours cell = length $ filter isEmptyCell (surroundingCells cell)
    filledCells = S.fromList $ boardFilled b
    isEmptyCell c@(Cell _ y) = y < 0 || (isOnBoard b c && not (c `S.member` filledCells))


averageCellY2 :: GameState -> Float
averageCellY2 = average . map (\c -> cellY c ^ 2) . boardFilled . gsBoard

average :: Integral i => [i] -> Float
average xs = fromIntegral (sum xs) / fromIntegral (length xs)


stateTreeUntil :: (GameState -> Bool) -> GameState -> Tree GameState
stateTreeUntil f gs = evalState (unfoldTreeM_BF (unfoldUntil f) gs) S.empty

unfoldUntil :: (GameState -> Bool) -> GameState -> State (Set [Cell]) (GameState, [GameState])
unfoldUntil f gs =
  do
    seen <- get
    if gsGameOver gs || f gs || (thisPosition `S.member` seen)
    then return (gs, [])
    else do
      put (thisPosition `S.insert` seen)
      return (gs, legalResults)
  where
    thisPosition = positionFingerprint gs
    legalResults = map snd $ filter legal possibleResults
    possibleResults = map (playCommand gs) allCommands
    legal (IllegalCommand, _) = False
    legal _ = True


positionFingerprint :: GameState -> [Cell]
positionFingerprint = sort . unitMembers . fromJust . gsCurrentUnit


nextMovesUntil :: (GameState -> Bool) -> GameState -> [GameState]
nextMovesUntil f gs = filter f $ walkTree (stateTreeUntil f gs)


nextUnitPlacements :: GameState -> [GameState]
nextUnitPlacements gs = nextMovesUntil ((unitsSoFar <) . gsUnitsPlaced) gs
  where
    unitsSoFar = gsUnitsPlaced gs


bestUnitPlacements :: GameState -> [GameState]
bestUnitPlacements gs = bestAdvancesFrom gs $ nextUnitPlacements gs


bestUnitPlacement :: GameState -> GameState
bestUnitPlacement = head . bestUnitPlacements


bestAdvancesFrom :: GameState -> [GameState] -> [GameState]
bestAdvancesFrom initial = sortBy (flip compare `on` sortKey)
  where sortKey s = (not (gsGameOver s), turnFitness initial s)


-- BFS
walkTree :: Tree GameState -> [GameState]
walkTree = concat . levels


bestMovesTree :: Int -> GameState -> Tree GameState
bestMovesTree width = unfoldTree nextMoves
  where
    nextMoves gs' = (gs',
                     if gsGameOver gs' then []
                     else take width $ bestUnitPlacements gs')


bestMoves :: Int -> Int -> GameState -> GameState
bestMoves width depth initial =
  head $ bestAdvancesFrom initial $ last $ take depth $ levels $ bestMovesTree width initial

bestUnitPlacementWithLookahead :: Int -> Int -> GameState -> GameState
bestUnitPlacementWithLookahead width depth gs = head $ bestAdvancesFrom gs $ concat $ take depth $ drop 1 $ levels $ bestMovesTree width gs


solver :: GameState -> GameState
solver game = head $ filter gsGameOver $ iterate (bestUnitPlacementWithLookahead 4 2) game
