module Executer where

import Control.Monad.State

import Types

getOpFunc :: OpType -> (Int -> Int -> Int)
getOpFunc ADD = (+)
getOpFunc SUB = (-)
getOpFunc MUL = (*)
getOpFunc DIV = (div)

evalExpression :: [Tree] -> State Int Int
evalExpression [] = do
  (n) <- get
  return (n)
evalExpression (tree : forest) = do
  (n) <- get
  case tree of
    (Leaf op n') -> put ((getOpFunc op) n n')
    (Node op forest') -> put ((getOpFunc op) n (execute forest'))
  evalExpression forest

execute :: [Tree] -> Int 
execute tree = evalState (evalExpression tree) 0
