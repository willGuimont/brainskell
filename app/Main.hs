{-# OPTIONS -Wall #-}

module Main where

import Control.Monad.Except
import System.Environment
import Text.ParserCombinators.Parsec

import Brainfuck
import Parsing

readBrainfuck :: String -> Either String BrainfuckProgram
readBrainfuck input =
  case parse parseBrainfuckProgram "brainfuck" input of
    Left err -> throwError $ show err
    Right x -> return x

evalBrainfuck :: Either String BrainfuckProgram -> IO ()
evalBrainfuck (Right x) = print x
evalBrainfuck (Left x) = print x

readAndEvalBrainfuck :: String -> IO ()
readAndEvalBrainfuck = evalBrainfuck . readBrainfuck

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-f", filename] -> do
      src <- readFile filename
      readAndEvalBrainfuck src
    [prog] -> readAndEvalBrainfuck prog
    _ -> putStrLn "Error expected -f or Brainfuck program"