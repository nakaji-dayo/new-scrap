{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module PracticeA where

import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  b <- T.getContents
  resolve b


resolve b = do
  let fst':snd':thr':_ = T.lines b
  T.putStrLn $ (tshow $ (tread fst' :: Int) + (sum $ tread <$> T.words snd')) <> " " <> thr'

tshow :: Show a => a -> Text
tshow = T.pack . show

tread :: Read a => Text -> a
tread = read . T.unpack


test = resolve "1\n2 3\ntest"
