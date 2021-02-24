{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module ABC086A where

import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  T.getLine >>= resolve

resolve l = do
  let a:b:_ = tread <$> T.words l :: [Int]
  if (a * b) `mod` 2 == 0
    then putStrLn "Even"
    else putStrLn "Odd"


---

tshow :: Show a => a -> Text
tshow = T.pack . show

tread :: Read a => Text -> a
tread = read . T.unpack
