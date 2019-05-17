module Executer where

import Control.Monad.State

import Types

getOpFunc :: OpType -> (Double -> Double -> Double)
getOpFunc ADD = (+)
getOpFunc SUB = (-)
getOpFunc MUL = (*)
getOpFunc DIV = (/)

evalExpression :: [Tree] -> State Double Double
evalExpression [] = do
  (n) <- get
  return (n)
evalExpression (tree : forest) = do
  (n) <- get
  case tree of
    (Leaf op n') -> put ((getOpFunc op) n n')
    (Node op forest') -> put ((getOpFunc op) n (execute forest'))
  evalExpression forest

execute :: [Tree] -> Double 
execute tree = evalState (evalExpression tree) 0
