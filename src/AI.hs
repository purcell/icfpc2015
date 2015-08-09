module AI where

import           Data.Foldable (maximumBy)
import           Data.Function (on)
import           Data.Tree
import           GamePlay
import           Types


stateTreeUntil :: (GameState -> Bool) -> GameState -> Tree GameState
stateTreeUntil f = unfoldTree (unfoldUntil f)

unfoldUntil :: (GameState -> Bool) -> GameState -> (GameState, [GameState])
unfoldUntil _ gs | gsGameOver gs = (gs, [])
unfoldUntil f gs | f gs          = (gs, [])
unfoldUntil _ gs                 = (gs, map snd $ filter legal possibleResults)
  where
    possibleResults = map (playCommand gs) (commandsForUnit $ gsCurrentUnit gs)
    legal (IllegalCommand, _) = False
    legal _ = True


-- Don't try to rotate 1-cell units
commandsForUnit :: Maybe Unit -> [Command]
commandsForUnit (Just (Unit [_] _)) = movementCommands
commandsForUnit _ = allCommands


highestScoring :: [GameState] -> GameState
highestScoring = maximumBy (compare `on` gsScore) -- TODO fitness includes whether game is now over


turnFitness :: GameState -> GameState -> Float
turnFitness initial final = turnScore * heavinessScore
  where turnScore = fromIntegral $ gsScore final - gsScore initial
        heavinessScore = averageCellY final - averageCellY initial


averageCellY :: GameState -> Float
averageCellY = average . map cellY . boardFilled . gsBoard
  where
    average xs = fromIntegral (sum xs) / fromIntegral (length xs)


nextMovesUntil :: (GameState -> Bool) -> GameState -> [GameState]
nextMovesUntil f gs = filter f $ walkTree (stateTreeUntil f gs)


bestUnitPlacement :: GameState -> GameState
bestUnitPlacement gs = maximumBy (compare `on` turnFitness gs) $ take 100 $ nextMovesUntil ((unitsSoFar <) . gsUnitsPlaced) gs
  where
    unitsSoFar = gsUnitsPlaced gs



-- BFS
walkTree :: Tree GameState -> [GameState]
walkTree = concat . levels


solver :: GameState -> GameState
solver game = head $ filter gsGameOver $ iterate bestUnitPlacement game

