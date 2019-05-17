module Tokenizer where

import Control.Monad.State
import Data.Char (isDigit)

import Types


-- split input string into list of strings "5 + 25" -> ["5", "+", "25"]
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
    "(" -> put (n + 1, tokens ++ [BEGIN_PARAN n])
    ")" -> put (n - 1, tokens ++ [END_PARAN (n - 1)])
    x   -> put (n, tokens ++ [NUM (read x :: Int)])
  (_, tokens') <- get
  return (tokens')

-- tokenize "5 + 2" -> [NUM 5, OP ADD, NUM 2]
tokenize :: String -> [Token]
tokenize string = evalState tokens (0, [])
  where
    lexlist = splitter string
    tokens = foldl (\x y -> x >> getToken y) (getToken . head $ lexlist) (tail lexlist)
