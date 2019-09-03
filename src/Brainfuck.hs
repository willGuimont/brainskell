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
  | Comment Char

instance Show Brainfuck where
  show = showBrainfuck

showBrainfuck :: Brainfuck -> String
showBrainfuck MoveRight = ">"
showBrainfuck MoveLeft = "<"
showBrainfuck Add = "+"
showBrainfuck Sub = "-"
showBrainfuck Print = "."
showBrainfuck Input = ","
showBrainfuck (Loop xs) = "[" ++ concatMap show xs ++ "]"
showBrainfuck (Comment _) = ""

newtype BrainfuckProgram = BrainfuckProgram [Brainfuck]

instance Show BrainfuckProgram where
  show (BrainfuckProgram xs)= concatMap show xs