module Run
  ( readBrainfuck
  , evalBrainfuck
  , readAndEvalBrainfuck
  , emptyTape
  ) where

import Control.Monad.Except
import Text.ParserCombinators.Parsec

import Brainfuck
import Evaluation
import Parsing

readBrainfuck :: String -> Either String Brainfuck
readBrainfuck input =
  case parse parseBrainfuckProgram "brainfuck" input of
    Left err -> throwError $ show err
    Right x -> return x

evalBrainfuck :: Tape -> Brainfuck -> IO ()
evalBrainfuck t b = eval b t >> pure ()

readAndEvalBrainfuck :: Tape -> String -> IO ()
readAndEvalBrainfuck t b =
  case readBrainfuck b of
    Left x -> print x
    Right x -> evalBrainfuck t x