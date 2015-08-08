{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import           Control.Exception   (assert)
import           Data.Maybe          (fromJust)
import           GamePlay
import           JSON
import           System.Environment  (getArgs)
import           Types


alwaysDown _ = Move SW

main :: IO ()
main = do
  file <- head <$> getArgs
  problem <- fromJust <$> parseProblemFromFile file
  let gameState = makeGameState problem (head (problemSourceSeeds problem))
      gameState' = runGame gameState alwaysDown
    in
    do
      print $ gameOver gameState'
      print $ gameBoardWithCurrentUnit gameState'
      print $ gsScore gameState'
