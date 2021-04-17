-- 16.9
module Deque (Deque, qHead, qTail, dqHead, dqTail) where 

newtype Deque a = DQ [a] 

qHead x (DQ [])     = (DQ [x])
qHead x (DQ xs)     = DQ (x:xs)

qTail x (DQ [])     = (DQ [x])
qTail x (DQ xs)     = DQ (xs++[x])

dqHead (DQ x:xs)    = DQ xs
dqTail (DQ xs)      = DQ (init xs)

