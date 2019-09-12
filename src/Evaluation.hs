{-# OPTIONS -Wall #-}

module Evaluation
  ( eval
  ) where

import Data.Array.IO
import Data.Char
import Data.IORef

import Brainfuck
import System.IO (hFlush, stdout)

writeTape :: IORef Tape -> Int -> IO ()
writeTape tRef value = do
  t <- readIORef tRef
  let xs = tape t
  i <- readIORef $ Brainfuck.index t
  writeArray xs i value
  writeIORef tRef t

readTape :: IORef Tape -> IO Int
readTape tRef = do
  t <- readIORef tRef
  let xs = tape t
  i <- readIORef $ Brainfuck.index t
  readArray xs i

modifyTape :: IORef Tape -> Int -> IO ()
modifyTape tRef change = do
  value <- readTape tRef
  writeTape tRef (value + change)

moveTape :: IORef Tape -> Int -> IO ()
moveTape tRef move = do
  t <- readIORef tRef
  let iRef = Brainfuck.index t
  i <- readIORef iRef
  writeIORef iRef (i + move)

readInt :: IO Int
readInt = readLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  res <- prompt
  if p res
    then return ()
    else action res >> until_ p prompt action

eval :: IORef Tape -> Brainfuck -> IO ()
eval tRef MoveRight = moveTape tRef 1
eval tRef MoveLeft = moveTape tRef (-1)
eval tRef Increment = modifyTape tRef 1
eval tRef Decrement = modifyTape tRef (-1)
eval tRef Print = readTape tRef >>= (\x -> putStr [chr x])
eval tRef Input = do
  putStr "Input: "
  hFlush stdout
  readInt >>= writeTape tRef
eval tRef (Loop x) = until_ (== 0) (readTape tRef) (\_ -> eval tRef x)
eval _ (Comment _) = return ()
eval tRef (Composed xs) = mapM_ (eval tRef) xs