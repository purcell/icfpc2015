{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import           Control.Exception   (assert)
import           Data.Maybe          (fromJust)
import           GamePlay
import           JSON
import           System.Environment  (getArgs)
import           Types


main :: IO ()
main = do
  file <- head <$> getArgs
  problem <- fromJust <$> parseProblemFromFile file
  let gameState' = foldl playCommand gameState [Move SW, Move SW, Move SW, Move SW, Move SW, Move SE] -- [Move SE, Move SE, Move SW, Move SW, Move SW, Move SE, Turn AntiClockwise]
      gameState = makeGameState problem (head (problemSourceSeeds problem))
    in
    do
      print $ gameOver gameState'
      print $ gameBoardWithCurrentUnit gameState'
      print $ gsScore gameState'
