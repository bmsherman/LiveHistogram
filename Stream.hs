module Stream where

import Control.Applicative ((<$>), Applicative (pure))
import System.IO (isEOF)

data Stream f a = a :< f (Stream f a) | NoMore

listToStream :: Applicative f => [a] -> Stream f a
listToStream [] = NoMore
listToStream (x : xs) = x :< pure (listToStream xs)

stdinStream :: IO (Stream IO Double)
stdinStream = do
  eof <- isEOF
  if eof then return NoMore else fmap (:< stdinStream) (read <$> getLine)
