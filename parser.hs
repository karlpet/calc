module Parser where

import Types

parseTree :: [Token] -> [Tree]
parseTree [] = []
parseTree ((OP op) : (NUM n) : tokens) = ((Leaf op n) : parseTree tokens)
parseTree ((OP op) : (BEGIN_PARAN n) : tokens) =
  ((Node op (parse inner)) : parseTree outer)
  where 
    inner = takeWhile ((/=) (END_PARAN n)) tokens
    outer = tail . dropWhile ((/=) (END_PARAN n)) $ tokens

parseTree (BEGIN_PARAN n : tokens) =
  ((Node ADD (parseTree inner)) : parseTree outer)
  where 
    inner = takeWhile ((/=) (END_PARAN n)) tokens
    outer = tail . dropWhile ((/=) (END_PARAN n)) $ tokens

precedenceScan :: [Tree] -> [Tree]
precedenceScan [] = []
precedenceScan (a : (Leaf MUL n) : forest) = [Node ADD (a : (Leaf MUL n) : precedenceScan forest)]
precedenceScan (a : (Leaf DIV n) : forest) = [Node ADD (a : (Leaf DIV n) : precedenceScan forest)]
precedenceScan (x : xs) = (x : precedenceScan xs)

parse :: [Token] -> [Tree]
parse tokens = precedenceScan . parseTree $ (OP ADD : tokens)