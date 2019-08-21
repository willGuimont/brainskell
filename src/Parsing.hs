{-# OPTIONS -Wall #-}

module Parsing
  ( parseBrainfuckProgram
  ) where

import Text.ParserCombinators.Parsec

import Brainfuck

parseRight :: Parser Brainfuck
parseRight = char '>' >> return MoveRight

parseLeft :: Parser Brainfuck
parseLeft = char '<' >> return MoveLeft

parseAdd :: Parser Brainfuck
parseAdd = char '+' >> return Add

parseSub :: Parser Brainfuck
parseSub = char '-' >> return Sub

parsePrint :: Parser Brainfuck
parsePrint = char '.' >> return Print

parseInput :: Parser Brainfuck
parseInput = char ',' >> return Input

parseLoop :: Parser Brainfuck
parseLoop = do
  _ <- char '['
  bs <- many parseBrainfuck
  end <- parseLoopEnd
  return $ Loop $ bs ++ [end]

parseLoopEnd :: Parser Brainfuck
parseLoopEnd = char ']' >> return LoopEnd

parseComment :: Parser Brainfuck
parseComment = Comment <$> anyChar

parseBrainfuck :: Parser Brainfuck
parseBrainfuck =
  parseRight <|> parseLeft <|> parseAdd <|> parseSub <|> parsePrint <|> parseInput <|> parseLoop <|> parseComment

parseBrainfuckProgram :: Parser BrainfuckProgram
parseBrainfuckProgram = do
  xs <- many parseBrainfuck
  return $ BrainfuckProgram xs