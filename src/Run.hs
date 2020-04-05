module Run
  ( readAndEvalBrainfuck
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

evalBrainfuck :: Tape -> Either String Brainfuck -> IO ()
evalBrainfuck t (Right x) = eval x t >> pure ()
evalBrainfuck _ (Left x) = print x

readAndEvalBrainfuck :: Tape -> String -> IO ()
readAndEvalBrainfuck t = evalBrainfuck t . readBrainfuck
