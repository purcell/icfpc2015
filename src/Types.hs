module Types where

------------------------------------------------------------------------------
-- Problem definitions
------------------------------------------------------------------------------

data Unit = Unit { unitMembers :: [Cell]
                 , unitPivot   :: Cell
                 } deriving Show

data Cell = Cell { cellX :: Int
                 , cellY :: Int
                 } deriving (Show, Eq)


data Problem = Problem { problemId           :: Int
                       , problemUnits        :: [Unit]
                       , problemWidth        :: Int
                       , problemHeight       :: Int
                       , problemFilled       :: [Cell]
                       , problemSourceLength :: Int
                       , problemSourceSeeds  :: [Int]
                       } deriving Show


