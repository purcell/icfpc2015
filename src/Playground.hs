module Playground where


import           AI
import           Control.Applicative ((<$>))
import           Data.Maybe          (fromJust)
import           GamePlay
import           JSON
import           Main
import           System.IO           (hPutStrLn, stderr)
import           Types




sampleGameState :: IO GameState
sampleGameState = do
  problem <- fromJust <$> parseProblemFromFile "problems/problem_0.json"
  return $ makeGameState problem 0




showResult :: (GameState -> GameState) -> IO GameState
showResult f = do
  game <- sampleGameState
  hPutStrLn stderr "BEFORE"
  hPutStrLn stderr "==========================================="
  dumpBoard game
  let game' = f game
    in do
      hPutStrLn stderr "==========================================="
      hPutStrLn stderr "AFTER"
      hPutStrLn stderr "==========================================="
      dumpBoard game'
      return game'
