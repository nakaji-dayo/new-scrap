{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module ABC087B where

import           Data.List    (sort)
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  T.getLine
  xs <- fmap tread . T.words <$> T.getLine  :: IO [Int]
  resolve xs

resolve xs = do
  let (_, a, b) = foldr f (True, 0, 0) $ sort xs
  print (a - b)
  where
    f x (isa, a, b)
      | isa = (False, a + x, b)
      | otherwise = (True, a, b + x)
---

tshow :: Show a => a -> Text
tshow = T.pack . show

tread :: Read a => Text -> a
tread = read . T.unpack
