import Stream (stdinStream)
import LiveHistogram

import UI.HSCurses.Curses

import Control.Exception (finally)

import Data.Char (isDigit)

import System.Console.GetOpt
import System.Environment (getArgs)

import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let (opts, nonopts, errs) = getOpt RequireOrder cmdOpts (processArgs args)
  case (Help `elem` opts, makeStdParams opts nonopts) of
    (True, _) -> showHelp
    (_, Just makeParams) -> do
      outputString <- flip finally endWin $ do
        initCurses
        params <- makeParams 
        stdinStream >>= histogram params
      putStrLn outputString
    (_, Nothing) -> showHelp
  where
  showHelp = putStrLn usageStr

makeDefaultParams :: (a, a) -> IO (Params a)
makeDefaultParams (l,h) = do
  height <- fmap (subtract 5 . fst) $ scrSize
  return $ Params (l,h) height Nothing 30

makeStdParams :: [Flag] -> [String] -> Maybe (IO (Params Double))
makeStdParams flags [l, h] = Just $ do
  defParams <- makeDefaultParams (read l, read h)
  return . foldr (.) id (map f flags) $ defParams
  where
  showEdges = ShowBinEdges `elem` flags
  f (NumBins x) = \p -> p { numBins = x }
  f ShowBinEdges = \p -> p { showBinEdges = Just (8, printf "%8.1e")  }
  f (RefreshRate x) = \p -> p { refreshRate = x }
  f _ = id
makeStdParams _ _ = Nothing

data Flag = ShowBinEdges | Help | NumBins Int | RefreshRate Integer deriving Eq

processArgs :: [String] -> [String]
processArgs [] = []
processArgs list@(x : xs) =
  if negNum x then "--" : list else x : processArgs xs
  where
  negNum ('-' : rest) = all isDigit rest
  negNum _ = False

cmdOpts :: [OptDescr Flag]
cmdOpts = 
  [ Option ['n'] ["nbins"] (ReqArg (NumBins . read) "x")
      "Use x histogram bins (default depends on terminal size)."
  , Option ['e'] ["edges"] (NoArg (ShowBinEdges))
      "Show the boundaries for each histogram bin."
  , Option ['r'] ["refresh"] (ReqArg (RefreshRate . read) "x")
      "Refresh screen at a rate of x Hz (default: 30 Hz)."
  , Option [] ["help"] (NoArg Help)
      "Display this help message."
  ]

usageStr = flip usageInfo cmdOpts
  "\n\
  \  Usage: \n\
  \ \n\
  \    LiveHistogram [options] min max \n\
  \ \n\
  \Display a live histogram and count/mean/standard deviation statistics for\n\
  \a stream of numbers from the standard input, where each number is on its \n\
  \own line. \n\
  \  min   : minimum value to be shown in the histogram \n\
  \  max   : maximum value to be shown in the histogram \n"
