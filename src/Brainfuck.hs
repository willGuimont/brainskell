{-# LANGUAGE TemplateHaskell #-}

module Brainfuck
  ( Brainfuck(..)
  , Tape(..)
  , mapTape
  , mapHead
  , readTape
  , emptyTape
  ) where

import Control.Lens
import qualified Data.Map.Strict as M

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
    { _tape :: M.Map Int Int
    , _headIndex :: Int
    }

makeLenses ''Tape

emptyTape :: Tape
emptyTape = Tape M.empty 0

mapTape :: (Int -> Int) -> Tape -> Tape
mapTape f t = set tape nm t
  where
    m = view tape t
    i = view headIndex t
    x = readTape t
    nm = set (at i) (Just $ f x) m

mapHead :: (Int -> Int) -> Tape -> Tape
mapHead = over headIndex

readTape :: Tape -> Int
readTape t = M.findWithDefault 0 i m
  where
    m = view tape t
    i = view headIndex t