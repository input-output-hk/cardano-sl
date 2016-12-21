{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Mockable.Channel (

      ChannelT
    , Channel(..)
    , newChannel
    , readChannel
    , tryReadChannel
    , unGetChannel
    , writeChannel

    ) where

import Mockable.Class

type family ChannelT (m :: * -> *) :: * -> *

data Channel (m :: * -> *) (t :: *) where
    NewChannel :: Channel m (ChannelT m t)
    ReadChannel :: ChannelT m t -> Channel m t
    TryReadChannel :: ChannelT m t -> Channel m (Maybe t)
    UnGetChannel :: ChannelT m t -> t -> Channel m ()
    WriteChannel :: ChannelT m t -> t -> Channel m ()

newChannel :: ( Mockable Channel m ) => m (ChannelT m t)
newChannel = liftMockable NewChannel

readChannel :: ( Mockable Channel m ) => ChannelT m t -> m t
readChannel channel = liftMockable $ ReadChannel channel

tryReadChannel :: ( Mockable Channel m ) => ChannelT m t -> m (Maybe t)
tryReadChannel channel = liftMockable $ TryReadChannel channel

unGetChannel :: ( Mockable Channel m ) => ChannelT m t -> t -> m ()
unGetChannel channel t = liftMockable $ UnGetChannel channel t

writeChannel :: ( Mockable Channel m ) => ChannelT m t -> t -> m ()
writeChannel channel t = liftMockable $ WriteChannel channel t
