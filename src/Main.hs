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
  print $ let board = createBoard problem in
    -- (head $ problemUnits problem)
    let unit = spawnUnit board (problemUnits problem !! 25) in
      lockUnit board (unitRotateClockwise (unitRotateClockwise unit))
