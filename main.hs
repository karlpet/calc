module Main where

import System.Environment   

import Tokenizer
import Parser
import Executer

calculate :: String -> Double
calculate = execute . parse . tokenize

main :: IO ()
main = do
  args <- getArgs
  print . calculate $ args !! 0
  return ()