module Main where

import System.Environment   

import Tokenizer
import Parser
import Executer

calculate :: String -> Int
calculate = execute . parse . tokenize

main :: IO ()
main = do
  args <- getArgs
  print . calculate $ args !! 0
  return ()