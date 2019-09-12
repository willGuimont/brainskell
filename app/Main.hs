{-# OPTIONS -Wall #-}

module Main where

import System.Environment

import Repl

main :: IO ()
main = do
  args <- getArgs
  t <- emptyTape
  s <-
    case args of
      ["-f", filename] -> return . Right $ readFile filename
      [prog] -> return $ Right $ pure prog
      _ -> return $ Left "Error expected -f or Brainfuck program"
  either print (>>= readAndEvalBrainfuck t) s