module Main where

import           AI
import           Control.Applicative ((<$>))
import           Data.Maybe          (fromJust)
import           GamePlay
import           JSON
import           System.Environment  (getArgs)
import           Types


main :: IO ()
main = do
  file <- head <$> getArgs
  problem <- fromJust <$> parseProblemFromFile file
  let
      seed = (head (problemSourceSeeds problem))
      gameState = makeGameState problem seed
      gameState' = runGame gameState bestStrategy
      solution = Solution (problemId problem) seed (map commandChar (gsCommandHistory gameState'))
    in
    do
      putStrLn $ encodeSolutions [solution]
      -- print $ gameOver gameState'
      -- print $ gameBoardWithCurrentUnit gameState'
      -- print $ gsScore gameState'
