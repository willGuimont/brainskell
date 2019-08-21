{-# OPTIONS -Wall #-}

module Main where

import Control.Monad.Except
import Text.ParserCombinators.Parsec

import Brainfuck
import Parsing

-- parse parseExpr "lisp" input
readBrainfuck :: String -> Either String BrainfuckProgram
readBrainfuck input =
  case parse parseBrainfuckProgram "brainfuck" input of
    Left err -> throwError $ show err
    Right x -> return x

{-
main :: IO ()
main = do
  args <- getArgs
  let evaluated = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaluated
-}
main :: IO ()
main = print (show <$> readBrainfuck "+++")