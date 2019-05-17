module Parser where

-- "(5 + 7)"         -> [(+) [(+) 5, (+) 7] 0] 0 
-- "(5 + 7) + 4"     -> [(+) [(+) 5, (+) 7], (+) 4] 0
-- "4 - (5 + 7) + 4" -> [(+) 4, (-) [(+) 5, (+) 7] 0, (+) 4] 0

-- 5 + 2 - (5 - (7 + 1)) + 5
-- ->
-- [(+) 5, (+) 2, (-) [(+) 5, (-) [(+) 7,(+) 1] 0] 0, (+) 5] 0

import Types

parseTree :: [Token] -> [Tree]
parseTree [] = []
parseTree ((OP op) : (NUM n) : tokens) = ((Leaf op n) : parseTree tokens)
parseTree ((OP op) : (BEGIN_PARAN n) : tokens) = ((Node op (parse inner)) : parseTree outer)
  where 
    inner = takeWhile ((/=) (END_PARAN n)) tokens
    outer = tail . dropWhile ((/=) (END_PARAN n)) $ tokens

parseTree (BEGIN_PARAN n : tokens) = ((Node ADD (parseTree inner)) : parseTree outer)
  where 
    inner = takeWhile ((/=) (END_PARAN n)) tokens
    outer = tail . dropWhile ((/=) (END_PARAN n)) $ tokens

parse :: [Token] -> [Tree]
parse tokens = parseTree (OP ADD : tokens)