module Main where

import           AI
import           Control.Applicative ((<$>))
import           Data.Maybe          (fromJust)
import           GamePlay
import           JSON
import           System.Environment  (getArgs)
import           Types


runSolution :: Problem -> Int -> IO Solution
runSolution problem seed = return $ Solution (problemId problem) seed (map commandChar (gsCommandHistory gameState'))
  where
       gameState = makeGameState problem seed
       gameState' = runGame gameState bestStrategy

-- print $ gameOver gameState'
-- print $ gameBoardWithCurrentUnit gameState'
-- print $ gsScore gameState'


main :: IO ()
main = do
  file <- head <$> getArgs
  problem <- fromJust <$> parseProblemFromFile file
  solutions <- mapM (runSolution problem) (problemSourceSeeds problem)
  putStrLn $ encodeSolutions solutions
