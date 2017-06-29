{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}

module Network.Broadcast.Relay.Simple
    ( simpleRelayer
    ) where

import           Universum
import           Data.Time.Units                    (Microsecond)

import           Mockable                           (Mockable, CurrentTime, currentTime)
import qualified Mockable.Concurrent                as Concurrent
import qualified Mockable.Channel                   as Channel
import           Node                               (NodeId)

simpleRelayer
    :: forall datum m .
       ( Mockable Channel.Channel m
       , Mockable Concurrent.Concurrently m
       , Mockable CurrentTime m
       )
    => (datum -> Maybe NodeId -> m (Set NodeId)) -- Where to send? Computed at enqueue time.
    -> m ( datum -> Maybe NodeId -> m () -- Enqueue
         , m (datum, Maybe NodeId, NodeId, Microsecond) -- Dequeue
         )
simpleRelayer getTargets = do
    queue <- Channel.newChannel

    let enqueue :: datum -> Maybe NodeId -> m ()
        enqueue msg provenance = do
            targets <- getTargets msg provenance
            enqueueTime <- currentTime
            forM_ (toList targets) (\target -> Channel.writeChannel queue (msg, provenance, target, enqueueTime))

    let dequeue  :: m (datum, Maybe NodeId, NodeId, Microsecond)
        dequeue = do
            (msg, provenance, target, enqueueTime) <- Channel.readChannel queue
            dequeueTime <- currentTime
            let delta = dequeueTime - enqueueTime
            pure $ (msg, provenance, target, delta)

    return (enqueue, dequeue)
