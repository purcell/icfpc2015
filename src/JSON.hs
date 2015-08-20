{-# LANGUAGE OverloadedStrings #-}
module JSON
       ( parseProblem
       , parseProblemFromFile
       , encodeSolutions
       ) where

import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad              (mzero)
import           Data.Aeson                 (FromJSON, ToJSON, decode, encode,
                                             object, parseJSON, (.:), (.=))
import qualified Data.Aeson.Types           as AT
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Types


------------------------------------------------------------------------------
-- Parse problem definitions from JSON
------------------------------------------------------------------------------

instance FromJSON Cell where
  parseJSON (AT.Object o) = Cell <$> o .: "x" <*> o .: "y"
  parseJSON _ = mzero

instance FromJSON Unit where
  parseJSON (AT.Object o) = Unit <$> o .: "members" <*> o .: "pivot"
  parseJSON _ = mzero

instance FromJSON Problem where
  parseJSON (AT.Object o) = Problem <$> o .: "id"
                                    <*> o .: "units"
                                    <*> o .: "width"
                                    <*> o .: "height"
                                    <*> o .: "filled"
                                    <*> o .: "sourceLength"
                                    <*> o .: "sourceSeeds"
  parseJSON _ = mzero

parseProblem :: ByteString -> Maybe Problem
parseProblem = decode

parseProblemFromFile :: FilePath -> IO (Maybe Problem)
parseProblemFromFile file = parseProblem <$> BSL.readFile file



------------------------------------------------------------------------------
-- Dump solution to JSON
------------------------------------------------------------------------------

instance ToJSON Solution where
  toJSON (Solution pid seed commands _) = object [ "problemId" .= pid
                                                 , "seed" .= seed
                                                 , "solution" .= commands
                                                 ]

encodeSolutions :: [Solution] -> String
encodeSolutions solutions = BSL.unpack $ encode solutions
