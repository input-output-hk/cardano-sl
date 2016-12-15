{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Mockable.Channel (

      ChannelT
    , Channel(..)
    , newChannel
    , readChannel
    , writeChannel

    ) where

import Mockable.Class

type family ChannelT (m :: * -> *) :: * -> *

data Channel (m :: * -> *) (t :: *) where
    NewChannel   :: Channel m (ChannelT m t)
    ReadChannel  :: ChannelT m t -> Channel m t
    WriteChannel :: ChannelT m t -> t -> Channel m ()

newChannel :: ( Mockable Channel m ) => m (ChannelT m t)
newChannel = liftMockable NewChannel

readChannel :: ( Mockable Channel m ) => ChannelT m t -> m t
readChannel channel = liftMockable $ ReadChannel channel

writeChannel :: ( Mockable Channel m ) => ChannelT m t -> t -> m ()
writeChannel channel t = liftMockable $ WriteChannel channel t
