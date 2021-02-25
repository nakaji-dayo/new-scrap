{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module ABC087B where

import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  a:b:c:x:_ <- fmap tread . T.lines <$> T.getContents :: IO [Int]
  resolve a b c x

resolve a b c x = do
  let xs = [() | na <- [0..a], nb <- [0..b], nc <- [0..c], na * 500 + nb * 100 + nc * 50 == x]
  print $ length xs

---

tshow :: Show a => a -> Text
tshow = T.pack . show

tread :: Read a => Text -> a
tread = read . T.unpack
