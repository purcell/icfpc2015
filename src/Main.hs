module Main where

import           AI
import           Control.Applicative ((<$>))
import           Data.Maybe          (fromJust)
import           GamePlay
import           JSON
import           System.Environment  (getArgs)
import           System.IO           (hPrint, stderr)
import           Types


runSolution :: Problem -> Int -> IO Solution
runSolution problem seed = do
  hPrint stderr $ gameBoardWithCurrentUnit gameState'
  return $ Solution (problemId problem) seed (map commandChar (gsCommandHistory gameState'))
  where
       gameState = makeGameState problem seed
       gameState' = runGame gameState bestStrategy



main :: IO ()
main = do
  file <- head <$> getArgs
  problem <- fromJust <$> parseProblemFromFile file
  solutions <- mapM (runSolution problem) (problemSourceSeeds problem)
  putStrLn $ encodeSolutions solutions
