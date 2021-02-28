{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module ABC193A where

import           Data.List    (minimum, sort)
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Numeric      (showFFloat)

-- main :: IO ()
-- main = do
--   a:b:_ <- fmap tread . T.words <$> T.getLine  :: IO [Float]
--   let r = (a - b) / a * 100
--   putStrLn $ showFFloat Nothing r ""

-- resolve xs = do
--   let (_, a, b) = foldr f (True, 0, 0) $ sort xs
--   print (a - b)
--   where
--     f x (isa, a, b)
--       | isa = (False, a + x, b)
--       | otherwise = (True, a, b + x)

data Store = Store Int Int Int
  deriving (Show, Eq)

main :: IO ()
main = do
  _:ls <- T.lines <$> T.getContents
  let ss = (\(a:p:x:_) -> Store a p x) . fmap tread . T.words <$> ls
  let r = (\(Store a p x) -> p) <$> filter (\(Store a p x) -> x - a > 0) ss
  print $ if not (null r) then minimum r else -1

-----
tshow :: Show a => a -> Text
tshow = T.pack . show

tread :: Read a => Text -> a
tread = read . T.unpack
