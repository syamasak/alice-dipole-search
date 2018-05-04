{-# LANGUAGE TemplateHaskell #-}
module FastDipole where

import           AliMagF
import           Control.Lens
import           Data.List

type FastDipole  = SegmentSearch FastDipoleX
type FastDipoleX = SegmentSearch FastDipoleY
type FastDipoleY = SegmentSearch [Int]

-- | Data structure for O(1) search of 'a'
data SegmentSearch a = SegmentSearch
  { _fBorders  :: [Double] -- Where segment borders are (ascending order)
  , _fDivision :: LineDivision -- How number line is devided evenly
  , _fSlices   :: Slices SegmentEnd -- Represent the devided line segments (some include end of segment)
  , _fSegments :: [a]  -- Segment specific data
  } deriving (Show, Read)

-- | A parameter of arithmetic sequence which slices target number line (e.g. X,Y,Z axis)
data LineDivision = LineDivision
  { _fNumDivision :: Int     -- The number of division
  , _fWidth       :: Double  -- The length
  , _fOffset      :: Double  -- The start position
  } deriving (Show, Read)

-- | Evenly divided line segments queriable by a real number
type Slices a = [a]

-- | End of a segment
data SegmentEnd = SegmentEnd
  { _segmentIx  :: Int     -- The [xyz]id of left-side segment
  , _segmentEnd :: Double  -- The position of left-side segment end or Infinity
  } deriving (Show, Read)

makeLenses ''SegmentSearch
makeLenses ''LineDivision
makeLenses ''SegmentEnd

-- | A 'SegmentEnd' without actual end of a segment.
continue :: Int -> SegmentEnd
continue = flip SegmentEnd (1/0) -- never match

-- | Filter given list of 'AliCheb3D' by condition f, then remove duplicates and sort by (axis)-th axis in acending order.
-- The f is a function that takes ([minX,minY,minZ], [maxX,maxY,maxZ]) and returns True or False.
patchBorders :: Int -> (([Double], [Double]) -> Bool) -> [(String, AliCheb3D)] -> [Double]
patchBorders axis f d =
  d ^.. traverse . _2 . region . filtered f . both . ix axis & nub & sort

-- | Generate an arithmetic sequence (slices) from 'LineDivision' spec.
makeGrid :: LineDivision -> [Double]
makeGrid (LineDivision n width offset) = [ (width / n') * k + offset | k <- [0..n'] ]
  where n' = realToFrac n

-- | Make a spec of arithmetic sequence from the border positions of patches and the number of divisions.
divBy :: [Double] -> Int -> LineDivision
divBy zs n = LineDivision n (last zs - head zs) (head zs)

-- | Enumerate all patch borders (as 'SegmentEnd') within every slice.
bordersInSlices :: [Double] -> [Double] -> [[SegmentEnd]]
bordersInSlices zs slices =
  [ zip [0..] (tail zs) ^.. traversed . filtered (\(_, z) -> l <= z && z < r) . to (uncurry SegmentEnd)
  | (l, r) <- zip slices (tail slices) ]

-- | Try to divide 'zs' (list of border z values) into n slices.
trySliceBy :: [Double] -> Int -> [[SegmentEnd]]
trySliceBy zs n = bordersInSlices zs (makeGrid (zs `divBy` n))

-- | Maximum number of patch borders in _one slice_, when the number of division is 'n'.
multiplicity :: [Double] -> Int -> Int
multiplicity zs n = maximum $ map length $ zs `trySliceBy` n

-- | Calculate the minimum number of division 'n' so that "multiplicity zs n == 1" holds.
-- One slice must not have more than two patch borders in it.
minDivisionN :: [Double] -> Int
minDivisionN zs = length (takeWhile (>1) $ map (multiplicity zs) [1..]) + 1

-- | Given the number of division 'n' (large enouth), get spec for such slices (as 'LineDivision'), and list of one patch border per slice.
sliceBy :: [Double] -> Int -> (LineDivision, [SegmentEnd])
sliceBy zs n = (zs `divBy` n, go 0 $ trySliceBy zs n)
  where
    go lastZId []                          = [] -- finish
    go lastZId ([] : xs)                   = continue lastZId : go lastZId xs -- no border here
    go _ ([x@(SegmentEnd leftZId _)] : xs) = x : go (leftZId + 1) xs -- pattern match error if there are many borders in a slice

-- | Slice given 'zs' so that the number of slices is minimum.
searchGoodSlice :: [Double] -> (LineDivision, [SegmentEnd])
searchGoodSlice zs = zs `sliceBy` minDivisionN zs

-- | 'zipWith' that gives adjacent two items of zs to 'f'.
--
-- > zipAdjacent [1,2,3,4] f == [f 1 2, f 2 3, f 3 4]
--
zipAdjacent :: [a] -> (a -> a -> b) -> [b]
zipAdjacent zs f = zipWith f zs (tail zs)

zipAdjacent' :: [a] -> ((a, a) -> b) -> [b]
zipAdjacent' zs f' = zipAdjacent zs (curry f')

isHalfCoveredBy :: (Double, Double) -> (Double, Double) -> Bool
isHalfCoveredBy segment@(smin, smax) patch@(pmin, pmax) =
  let smid = (smin + smax) / 2 in pmin <= smid && smid <= pmax

-- | Enumerate X-axis border positions of the patches which are crossing over given Z segment (minZ, maxZ).
yPatchBorders :: (Double, Double) -> [(String, AliCheb3D)] -> [Double]
yPatchBorders zseg = patchBorders 1{- Y axis -} $
  \([_,_,z],[_,_,z']) -> zseg `isHalfCoveredBy` (z, z')

-- | All Y-axis border positions, where both Z in (minZ, maxZ) AND X in (minX, maxX) is satisfied.
xPatchBorders :: (Double, Double) -> (Double, Double) -> [(String, AliCheb3D)] -> [Double]
xPatchBorders zseg yseg = patchBorders 0{- X axis-} $
  \([x,y,z],[x',y',z']) -> zseg `isHalfCoveredBy` (z, z')
                       && yseg `isHalfCoveredBy` (y, y')

-- | Turn AliMagF Dipole into FastDipole!
fastDipoleSegs :: [(String, AliCheb3D)] -> FastDipole
fastDipoleSegs dip =
  let zs = patchBorders 2{- Z axis -} (const True) dip
      (dv, slices) = searchGoodSlice zs
  in SegmentSearch zs dv slices $ zipAdjacent' zs $ \zseg ->
      let ys = yPatchBorders zseg dip
          (dv', slices') = searchGoodSlice ys
      in SegmentSearch ys dv' slices' $ zipAdjacent' ys $ \yseg ->
          let xs = xPatchBorders zseg yseg dip
              (dv'', slices'') = searchGoodSlice xs
          in SegmentSearch xs dv'' slices'' $ zipAdjacent' xs $ \xseg ->
              map fst $ filter (\(i,([x,y,z],[x',y',z'])) ->
                   zseg `isInsideOf` (z, z')
                && yseg `isInsideOf` (y, y')
                && xseg `isInsideOf` (x, x')
              ) $ zip [0..] $ dip ^.. traversed . _2 . region
  where isInsideOf (smin, smax) (zmin, zmax) = let smid = (smin + smax) / 2 in zmin < smid && smid < zmax

-- | Fast search of the unique segment which includes given 1D point
quickSearch :: SegmentSearch a -> Double -> Int
quickSearch ss z = segmentIx + if z < segmentEnd then 0 else 1
  where SegmentEnd segmentIx segmentEnd = _fSlices ss !! index (_fDivision ss)
        index (LineDivision n w d) = floor ((z - d) / w * realToFrac n)

-- | Fast search of the regions which includes given 3D point
dipoleSearch :: FastDipole -> (Double, Double, Double) -> [Int]
dipoleSearch zDip (x,y,z) =
  let zix = quickSearch zDip z
      xDip = _fSegments zDip !! zix
      xix = quickSearch xDip y
      yDip = _fSegments xDip !! xix
      yix = quickSearch yDip x
      paramIx = _fSegments yDip !! yix
  in paramIx
