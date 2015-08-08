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
  let board = createBoard problem
      unit = spawnUnit board (problemUnits problem !! 25)
      unit' = foldl (flip applyRawCommand) unit [Move E, Move SE, Turn AntiClockwise]
    in
    print $ lockUnit board unit'
