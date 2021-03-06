module Types where

data OpType = ADD | SUB | MUL | DIV
  deriving(Show, Eq)

data Token = NUM Double | OP OpType | BEGIN_PARAN Int | END_PARAN Int
  deriving(Show, Eq)

data Tree = Leaf OpType Double | Node OpType [Tree]
  deriving (Show, Eq)
