module AI where

import           Control.Monad.State.Lazy
import           Data.Foldable            (maximumBy)
import           Data.Function            (on)
import           Data.List                (sort)
import           Data.Maybe               (fromJust)
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Tree
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
turnFitness initial final = turnScore + heavinessScore - gameOverPenalty
  where turnScore = fromIntegral $ gsScore final - gsScore initial
        heavinessScore = averageCellY final
        gameOverPenalty = if gsGameOver final then 1000
                          else 0


averageCellY :: GameState -> Float
averageCellY = average . map cellY . boardFilled . gsBoard
  where
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


bestUnitPlacement :: GameState -> GameState
bestUnitPlacement gs = maximumBy (compare `on` turnFitness gs) $ take 100 $ nextMovesUntil ((unitsSoFar <) . gsUnitsPlaced) gs
  where
    unitsSoFar = gsUnitsPlaced gs



-- BFS
walkTree :: Tree GameState -> [GameState]
walkTree = concat . levels


solver :: GameState -> GameState
solver game = head $ filter gsGameOver $ iterate bestUnitPlacement game

