module Types where

------------------------------------------------------------------------------
-- Problem definitions
------------------------------------------------------------------------------

data Unit = Unit { unitMembers :: [Cell]
                 , pivot       :: Cell
                 } deriving Show

data Cell = Cell { cellX :: Integer
                 , cellY :: Integer
                 } deriving Show


data Problem = Problem { problemId           :: Integer
                       , problemUnits        :: [Unit]
                       , problemWidth        :: Integer
                       , problemHeight       :: Integer
                       , problemFilled       :: [Cell]
                       , problemSourceLength :: Integer
                       , problemSourceSeeds  :: [Integer]
                       } deriving Show
