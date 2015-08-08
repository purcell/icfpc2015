{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import           Data.Maybe          (fromJust)
import           GamePlay            (createGame)
import           JSON
import           System.Environment  (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  parsed <- parseProblemFromFile file
  print $ fromJust $ createGame <$> parsed
