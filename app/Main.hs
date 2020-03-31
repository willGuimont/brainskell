module Main where

import System.Environment

import Run

main :: IO ()
main = do
  args <- getArgs
  s <-
    case args of
      ["-f", filename] -> return . Right $ readFile filename
      [prog] -> return $ Right $ pure prog
      _ -> return $ Left "Error expected -f or Brainfuck program"
  either print (>>= readAndEvalBrainfuck emptyTape) s