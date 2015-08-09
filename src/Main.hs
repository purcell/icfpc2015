module Main where

import           AI
import           Control.Applicative ((<$>))
import           Data.Maybe          (fromJust)
import           GamePlay
import           JSON
import           System.Environment  (getArgs)
import           System.IO           (hPrint, hPutStrLn, stderr)
import           Types



dumpBoard :: GameState -> IO ()
dumpBoard gs = hPrint stderr $ gameBoardWithCurrentUnit gs


runSolution :: Problem -> Int -> IO Solution
runSolution problem seed = do
  dumpBoard gameState'
  return $ Solution (problemId problem) seed (map commandChar (gsCommandHistory gameState'))
  where
       gameState = makeGameState problem seed
       gameState' = runGame gameState bestStrategy


testCommands :: FilePath -> Int -> [Command] -> IO ()
testCommands file seed commands = do
  problem <- fromJust <$> parseProblemFromFile file
  let gameState = makeGameState problem seed in
    do
      hPutStrLn stderr "========================================================="
      hPutStrLn stderr "Empty board"
      hPrint stderr $ gsBoard gameState
      hPutStrLn stderr "---------------------------------------------------------"
      dumpBoard gameState
      printCommands gameState commands


printCommands :: GameState -> [Command] -> IO ()
printCommands gs [] = return ()
printCommands gs (c:cs) = do
  hPutStrLn stderr "---------------------------------------------------------"
  hPutStrLn stderr $ show c ++ " ==> " ++ show result
  hPutStrLn stderr $ "Game over: " ++ show (gsGameOver gs')
  hPutStrLn stderr "---------------------------------------------------------"
  dumpBoard gs'
  printCommands gs' cs
  where (result, gs') = playCommand gs c


main :: IO ()
main = do
  file <- head <$> getArgs
  problem <- fromJust <$> parseProblemFromFile file
  solutions <- mapM (runSolution problem) (problemSourceSeeds problem)
  putStrLn $ encodeSolutions solutions
