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
  bs <- between (char '[') (char ']') (many parseBrainfuck)
  return $ Loop bs

parseComment :: Parser Brainfuck
parseComment = Comment <$> noneOf "<>[]+-.,"

parseBrainfuck :: Parser Brainfuck
parseBrainfuck =
  parseRight <|> parseLeft <|> parseAdd <|> parseSub <|> parsePrint <|> parseInput <|> parseLoop <|> parseComment

parseBrainfuckProgram :: Parser BrainfuckProgram
parseBrainfuckProgram = do
  code <- many parseBrainfuck
  return $ BrainfuckProgram code