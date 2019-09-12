{-# OPTIONS -Wall #-}

module Brainfuck where

import Data.Array.IO
import Data.IORef

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
    { tape :: IOArray Int Int
    , index :: IORef Int
    }