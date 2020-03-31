{-# LANGUAGE TemplateHaskell #-}

module Brainfuck
  ( Brainfuck(..)
  , Tape(..)
  , mapTape
  , mapIndex
  , readTape
  ) where

import Control.Lens
import Data.Maybe (fromMaybe)

data Brainfuck
  = MoveRight
  | MoveLeft
  | Increment
  | Decrement
  | Print
  | Input
  | Loop Brainfuck
  | Comment Char
  | Composed [Brainfuck]

instance Show Brainfuck where
  show = showBrainfuck

showBrainfuck :: Brainfuck -> String
showBrainfuck MoveRight = ">"
showBrainfuck MoveLeft = "<"
showBrainfuck Increment = "+"
showBrainfuck Decrement = "-"
showBrainfuck Print = "."
showBrainfuck Input = ","
showBrainfuck (Loop xs) = "[" ++ show xs ++ "]"
showBrainfuck (Comment _) = ""
showBrainfuck (Composed xs) = concatMap show xs

data Tape =
  Tape
    { _tape :: [Int]
    , _headIndex :: Int
    }

makeLenses ''Tape

mapTape :: (Int -> Int) -> Tape -> Tape
mapTape f t = set tape nm t
  where
    m = view tape t
    i = view headIndex t
    nm = over (element i) f m

mapIndex :: (Int -> Int) -> Tape -> Tape
mapIndex = over headIndex

readTape :: Tape -> Int
readTape t = fromMaybe 0 $ m ^? element i
  where
    m = view tape t
    i = view headIndex t