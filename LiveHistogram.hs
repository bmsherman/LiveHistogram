module LiveHistogram where

import Stream

import UI.HSCurses.Curses

import Data.Array.IArray
import Data.Array.IO
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

import System.CPUTime

data Params a = Params 
  { histBounds :: !(a,a)
  , numBins :: !Int
  , showBinEdges :: !(Maybe (Int, a -> String))
  , refreshRate :: !Integer
  }

binarySearch :: (IArray a e, Ord e) => a Int e -> e -> Int
binarySearch arr = go (bounds arr) where
  go (a,b) x | a >= b    = if x < arr ! a then a else succ a
             | otherwise = case middle (a,b) of
    m -> go (if x < arr ! m then (a,pred m) else (succ m,b)) x
  middle (l,h) = (l + h) `div` 2

data Stats a = Stats !Int !a !a !a !a

initStats :: Num e => e -> Stats e
initStats x = Stats 1 x 0 x x

updateStats :: (Ord e, Fractional e) => e -> Stats e -> Stats e
updateStats x (Stats total mean m2 smallest biggest) =
  Stats total' mean' m2' smallest' biggest'
  where
  total' = succ total
  mean' = mean + delta / fromIntegral total'
  m2' = m2 + delta * (x - mean')
  smallest' = min smallest x
  biggest' = max biggest x
  delta = x - mean

arraysForBins :: (Ord e, Fractional e) 
  => (e,e) -> Int -> (Array Int e, IO (IOUArray Int Int))
arraysForBins (l, h) nbins = (searchArr, countsArr) where
  bds = iterate (+step) l
  step = (h - l) / fromIntegral nbins
  searchArr = listArray (0, nbins) bds
  countsArr = newArray (0, succ nbins) 0

updateCounts :: (Ord e, Fractional e) =>
  Array Int e -> IOUArray Int Int -> e -> IO ()
updateCounts searchArr counts x = case binarySearch searchArr x of
  i -> readArray counts i >>= writeArray counts i . succ

binIdents :: (a -> String) -> [a] -> [String]
binIdents shower xs = zipWith f xs (tail xs) where
  f x y = shower x ++ " to " ++ shower y

asciiInfo :: (Show a, Floating a) 
  => Int -> Maybe (Int, a -> String) -> Stats a -> [a] -> [Int] -> String
asciiInfo width showBars stats bins counts = 
  intercalate "\n" . (++ asciiStats stats below above) $ case showBars of
    Nothing -> asciiBars width stats inRange
    Just (dwidth, shower) -> let binEdgesWidth = 2*dwidth + 5 in
      zipWith (\x y -> x ++ ' ':y)
        (replicate (binEdgesWidth-1) ' ' : binIdents shower bins)
        (asciiBars (width - binEdgesWidth) stats inRange)
  where
  (below, inRange, above) = stripEnds counts
  stripEnds xs = (head xs, (init . tail) xs, last xs)

asciiStats :: (Show e, Floating e) => Stats e -> Int -> Int -> [String]
asciiStats (Stats total mean m2 smallest biggest) below above = 
  zipWith (\i xs -> concatMap (extendWithR ' ' i) xs) [24, 36, 36]
  [ [  "Count    : " ++ show total
    ,  "Below    : " ++ show below
    ,  "Above    : " ++ show above]
  , [  "Mean     : " ++ show mean
     , "Std. Dev.: " ++ showMaybe maybeStdDev]
  , [  "Min      : " ++ show smallest
    ,  "Max      : " ++ show biggest]
  ]
  where
  showMaybe (Just x) = show x
  showMaybe (Nothing) = "N/A"
  maybeStdDev = if total < 2 then Nothing
    else Just $ sqrt (m2 / fromIntegral (pred total))

extendWithR :: a -> Int -> [a] -> [a]
extendWithR c n [] = replicate n c
extendWithR c n (x : xs) = x : extendWithR c (pred n) xs

extendWithL :: a -> Int -> [a] -> [a]
extendWithL c n xs = replicate (n - length xs) c ++ xs

asciiBars :: (Show e, Floating e) => Int -> Stats e -> [Int] -> [String]
asciiBars width (Stats total mean m2 _ _) counts = countsBar :
  [ replicate (round (fromIntegral width * n)) '#' | (i, n) <- zip [0..] normNums ]
  where
  left = width `div` 2
  right = width - left - 1
  countsBar = extendWithR ' ' left "| <-- 0" ++ '|' : extendWithL ' ' right (show maxBin ++ " --> |")
  maxBin = maximum counts
  normNums :: [Float]
  normNums = map ((/fromIntegral maxBin) . fromIntegral) counts


histogram :: (Ord a, RealFrac a, Floating a, Show a) 
  => Params a -> Stream IO a -> IO String
histogram params NoMore = return "No input data."
histogram params (x :< xs) = do
  wclear window
  countsArr <- makeCountsArr
  stt <- xs >>= go (-timeStep) (initStats x) countsArr
  getAsciiInfo stt
  where
  (searchArr, makeCountsArr) = arraysForBins (histBounds params) (numBins params)
  window = stdScr
  oneSecond = 1000000000000
  timeStep = oneSecond `div` refreshRate params

  getAsciiInfo (stats, counts) = do
    (_, w) <- scrSize
    return $ asciiInfo (pred w) (showBinEdges params) stats (elems searchArr) counts

  printState stt = do
    erase
    getAsciiInfo stt >>= mvWAddStr window 0 0
    wMove window 0 0
    wRefresh window

  go i stats countsArr ys = do
    i' <- getCPUTime
    nexti <- if (i' - i > timeStep)
       then do
         counts <- getElems countsArr
         printState (stats, counts) >> return i'
       else return i
    case ys of
      NoMore -> do
        counts <- getElems countsArr
        return (stats, counts)
      (z :< zs) -> do
         updateCounts searchArr countsArr z
         zs >>= go nexti (updateStats z stats) countsArr

