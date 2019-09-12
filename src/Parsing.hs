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
parseAdd = char '+' >> return Increment

parseSub :: Parser Brainfuck
parseSub = char '-' >> return Decrement

parsePrint :: Parser Brainfuck
parsePrint = char '.' >> return Print

parseInput :: Parser Brainfuck
parseInput = char ',' >> return Input

parseLoop :: Parser Brainfuck
parseLoop = do
  bs <- between (char '[') (char ']') parseComposed
  return $ Loop bs

parseComment :: Parser Brainfuck
parseComment = Comment <$> noneOf "<>[]+-.,"

parseComposed :: Parser Brainfuck
parseComposed = Composed <$> many parseSimple

parseSimple :: Parser Brainfuck
parseSimple =
  parseRight <|> parseLeft <|> parseAdd <|> parseSub <|> parsePrint <|> parseInput <|> parseLoop <|> parseComment

parseBrainfuckProgram :: Parser Brainfuck
parseBrainfuckProgram = parseComposed