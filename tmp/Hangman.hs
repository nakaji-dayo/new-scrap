module Hangman where



import           Control.Applicative      ((<|>))
import           Control.Monad            (forM_, when)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State.Lazy (MonadState (get, put),
                                           MonadTrans (lift),
                                           StateT (runStateT), gets)
import           Data.Maybe               (fromMaybe, isNothing)
hangman = do
  c <- readFile "./words.txt"
  -- todo: random
  f (lines c)
  where
    f [] = putStrLn "empty"
    f (x:xs) = do
      runStateT (process x) (replicate (length x) Nothing)
      putStrLn "play again?"
      x <- getLine
      when (x == "yes") $ f xs

process :: String -> StateT [Maybe Char] IO ()
process target = do
  opened <- get
  lift $ do
    printClosed opened
    putStrLn "guess?"
  i <- lift getLine
  if length i /= length target
    then ( do
             lift (putStrLn "wrong number")
             process target
         )
    else ( do
             let opened' = uncurry (<|>) <$> zip (fmap (open i) target) opened
             lift $ printClosed opened'
             put opened'
             when (any isNothing opened') $ process target
         )
  where
    open i x
      | x `elem` i = Just x
      | otherwise = Nothing
    printClosed xs = putStrLn $ fromMaybe '_' <$> xs
