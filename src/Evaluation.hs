module Evaluation
  ( eval
  ) where

import Data.Char
import Data.List

import Brainfuck
import Control.Monad (foldM)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

readInput :: IO (Maybe Int)
readInput = do
  x <- getLine
  pure $
    if "'" `isPrefixOf` x
      then Just $ ord (x !! 1)
      else readMaybe x

loopUntil :: Monad m => (t -> Bool) -> (t -> m t) -> t -> m t
loopUntil p f t =
  if p t
    then pure t
    else f t >>= loopUntil p f

bimapMaybe :: (Monad m) => Maybe a -> m b -> (a -> m b) -> m b
bimapMaybe Nothing f _ = f
bimapMaybe (Just x) _ g = g x

eval :: Brainfuck -> Tape -> IO Tape
eval MoveRight t = pure $ mapHead (+ 1) t
eval MoveLeft t = pure $ mapHead (subtract 1) t
eval Increment t = pure $ mapTape (+ 1) t
eval Decrement t = pure $ mapTape (subtract 1) t
eval Print t = (putChar . chr . readTape) t >> hFlush stdout >> pure t
eval Input t = do
  putStr "Input: "
  hFlush stdout
  val <- readInput
  bimapMaybe val (eval Input t) (\x -> pure $ mapTape (const x) t)
eval (Loop x) t = loopUntil ((0 ==) . readTape) (eval x) t
eval (Comment _) t = pure t
eval (Composed xs) t = foldM (flip eval) t xs