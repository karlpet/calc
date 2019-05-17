module Executer where

-- ... from parser "5 + 2 - (5 - (7 + 1)) + 5"

-- [(+) 5, (+) 2, (-) [(+) 5, (-) [(+) 7,(+) 1] 0] 0, (+) 5] 0
-- [(+) 2, (-) [(+) 5, (-) [(+) 7, (+) 1] 0] 0, (+) 5] 5
-- [(-) [(+) 5, (-) [(+) 7, (+) 1] 0] 0, (+) 5] 7
-- [(-) [(-) [(+) 7, (+) 1] 0] 5, (+) 5] 7 
-- [(-) [(-) 8] 5, (+) 5] 7
-- [(-) (-3), (+) 5] 7
-- [(+) 5] 10
-- 15

import Control.Monad.State

import Types

getOpFunc :: OpType -> (Int -> Int -> Int)
getOpFunc ADD = (+)
getOpFunc SUB = (-)

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
