module Rotation
       ( rotateClockwiseAround
       , rotateAntiClockwiseAround
       ) where

import           Data.Set (Set)
import qualified Data.Set as S
import           Types    (Cell (..))


------------------------------------------------------------------------------
-- Cell rotation
-- http://www.redblobgames.com/grids/hexagons/#rotation
------------------------------------------------------------------------------

data CubeCoord = CubeCoord Int Int Int
                 deriving (Eq, Ord)
data OffsetRCoord = OffsetRCoord Int Int

offsetRToCubeCoord :: OffsetRCoord -> CubeCoord
offsetRToCubeCoord (OffsetRCoord x y) = CubeCoord cubeX (- cubeX - y) y
  where cubeX = x - (y - (y `mod` 2)) `div` 2

cubeToOffsetRCoord :: CubeCoord -> OffsetRCoord
cubeToOffsetRCoord (CubeCoord x _ z) = OffsetRCoord (x + (z - (z `mod` 2)) `div` 2) z

addOffset :: CubeCoord -> CubeCoord -> CubeCoord
addOffset (CubeCoord offX offY offZ) (CubeCoord x y z) = CubeCoord (x + offX) (y + offY) (z + offZ)

calcOffset :: CubeCoord -> CubeCoord -> CubeCoord
calcOffset (CubeCoord ax ay az) (CubeCoord bx by bz) = CubeCoord (bx - ax) (by - ay) (bz - az)


rotateWith :: (CubeCoord -> CubeCoord) -> Cell -> Set Cell -> Set Cell
rotateWith rotator pivot cells = S.map cellFromRelativeCoord rotatedCoords
  where
    rotatedCoords = S.map rotator relativeCoords
    cellFromRelativeCoord coord = cubeCoordToCell $ addOffset pivotCoord coord
    relativeCoords = S.map (calcOffset pivotCoord . cellToCubeCoord) cells
    pivotCoord = cellToCubeCoord pivot
    cellToCubeCoord (Cell x y) = offsetRToCubeCoord $ OffsetRCoord x y
    cubeCoordToCell coord = Cell x y
      where OffsetRCoord x y = cubeToOffsetRCoord coord

rotateClockwiseAround :: Cell -> Set Cell -> Set Cell
rotateClockwiseAround = rotateWith rotateCoordClockwise
  where rotateCoordClockwise (CubeCoord x y z) = CubeCoord (-z) (-x) (-y)

rotateAntiClockwiseAround :: Cell -> Set Cell -> Set Cell
rotateAntiClockwiseAround = rotateWith rotateCoordAntiClockwise
  where rotateCoordAntiClockwise (CubeCoord x y z) = CubeCoord (-y) (-z) (-x)


