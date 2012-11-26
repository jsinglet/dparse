module Queue where

import DTypes

-- | Some Queue Functions
queue :: [Node]
queue = []

enqueue :: Node -> [Node] -> [Node]
enqueue n q = [n] ++ q

dequeue :: [Node] -> (Node, [Node])
dequeue q = (head q, tail q)



