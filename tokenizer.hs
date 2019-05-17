module Tokenizer where

import Control.Monad.State
import Data.Char (isDigit)

import Types

splitter :: String -> [String]
splitter "" = []
splitter str = if length digits > 0 then
    (digits : splitter digitsremain)
  else
    if head str == ' ' then
      splitter (tail str)
    else
      ([head str] : splitter (tail str))
  where
    digits = takeWhile isDigit str
    digitsremain = dropWhile isDigit str
     
getToken :: String -> State (Int, [Token]) [Token]
getToken str = do
  (n, tokens) <- get
  case str of
    "+" -> put (n, tokens ++ [OP ADD])
    "-" -> put (n, tokens ++ [OP SUB])
    "*" -> put (n, tokens ++ [OP MUL])
    "/" -> put (n, tokens ++ [OP DIV])
    "(" -> put (n + 1, tokens ++ [BEGIN_PARAN n])
    ")" -> put (n - 1, tokens ++ [END_PARAN (n - 1)])
    x   -> put (n, tokens ++ [NUM (read x :: Int)])
  (_, tokens') <- get
  return (tokens')

tokenize :: String -> [Token]
tokenize string = tokens
  where
    lexlist = splitter string
    states = foldl (\x y -> x >> getToken y) (getToken . head $ lexlist) (tail lexlist)
    tokens = evalState states (0, [])