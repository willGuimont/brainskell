{-# OPTIONS -Wall #-}

module Brainfuck where

data Brainfuck
  = MoveRight
  | MoveLeft
  | Add
  | Sub
  | Print
  | Input
  | Loop [Brainfuck]
  | LoopEnd
  | Comment Char

instance Show Brainfuck where
  show = showBrainfuck

instance Show BrainfuckProgram where
  show (BrainfuckProgram xs) = showListBrainFuck xs

showBrainfuck :: Brainfuck -> String
showBrainfuck MoveRight = ">"
showBrainfuck MoveLeft = "<"
showBrainfuck Add = "+"
showBrainfuck Sub = "-"
showBrainfuck Print = "."
showBrainfuck Input = ","
showBrainfuck (Loop xs) = "[" ++ show xs
showBrainfuck LoopEnd = "]"
showBrainfuck (Comment c) = [c]

showListBrainFuck :: [Brainfuck] -> String
showListBrainFuck xs = foldr1 (++) $ map show xs

newtype BrainfuckProgram =
  BrainfuckProgram [Brainfuck]

data BrainfuckInterpreter =
  BrainfuckInterpreter Integer [Integer]