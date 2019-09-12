{-# OPTIONS -Wall #-}

module Run
  ( readAndEvalBrainfuck
  , emptyTape
  ) where

import Control.Monad.Except
import Data.Array.IO
import Data.IORef
import Text.ParserCombinators.Parsec

import Brainfuck
import Evaluation
import Parsing

readBrainfuck :: String -> Either String Brainfuck
readBrainfuck input =
  case parse parseBrainfuckProgram "brainfuck" input of
    Left err -> throwError $ show err
    Right x -> return x

evalBrainfuck :: IORef Tape -> Either String Brainfuck -> IO ()
evalBrainfuck tRef (Right x) = eval tRef x
evalBrainfuck _ (Left x) = print x

readAndEvalBrainfuck :: IORef Tape -> String -> IO ()
readAndEvalBrainfuck t = evalBrainfuck t . readBrainfuck

emptyTape :: IO (IORef Tape)
emptyTape = do
  arr <- newArray (0, 30000) 0
  i <- newIORef 0
  newIORef $ Tape arr i