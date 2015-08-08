module GamePlay (createGame) where

import           Data.List (intercalate)
import           Types

------------------------------------------------------------------------------
-- Board inspection and manipulation
------------------------------------------------------------------------------


data Board = Board { boardWidth  :: Int
                   , boardHeight :: Int
                   , boardFilled :: [Cell]
                   }

instance Show Board where
  show board = intercalate "\n" $ map showRow (boardYs board)
    where showRow y = (if odd y then " " else "") ++ unwords [showCell x y | x <- boardXs board]
          showCell x y = if isOccupied board x y then "⬢" else "⬡"


boardXs :: Board -> [Int]
boardXs board = [0..(boardWidth board - 1)]
boardYs :: Board -> [Int]
boardYs board = [0..(boardHeight board - 1)]

isOccupied :: Board -> Int -> Int -> Bool
isOccupied board x y = Cell x y `elem` boardFilled board

------------------------------------------------------------------------------
-- Overall game state
------------------------------------------------------------------------------

data Game = Game { gameBoard :: Board }

instance Show Game where
  show = show . gameBoard




------------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------------

createGame :: Problem -> Game
createGame p = Game (Board (problemWidth p) (problemHeight p) (problemFilled p))

