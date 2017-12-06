{-|
Module:      Pos.Util.Queue
Description: A simple functional queue

This impl is ok and is O(1) amortised time provided that the use of
dequeue is non-persistent style, meaning that we never call dequeue
twice on the same value. In the case of
'Pos.Util.Concurrent.PriorityLock', where this is intended to be used,
that's true because it will be the unlocking thread that does the
dequeue.

An alternative impl that would be suitable in other cases would do the
reversing on the enqueue side.
-}
module Pos.Util.Queue
    ( Q
    , queue
    , enqueue
    , dequeue
    ) where

import           Universum

data Q a = Q [a] [a]

queue :: [a] -> Q a
queue xs = Q xs []

enqueue :: Q a -> a -> Q a
enqueue (Q front  back) x = Q front (x : back)

dequeue :: Q a -> Maybe (a, Q a)
dequeue (Q (x:xs) back)    = Just (x, Q xs back)
dequeue (Q []     back)    = case reverse back of
                               x:xs -> Just (x, Q xs [])
                               []   -> Nothing
