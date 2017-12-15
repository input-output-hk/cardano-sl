{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Mockable.Channel (

      ChannelT
    , Channel(..)
    , newChannel
    , readChannel
    , tryReadChannel
    , unGetChannel
    , writeChannel

    ) where

import           Mockable.Class (MFunctor' (hoist'), Mockable (liftMockable))

type family ChannelT (m :: * -> *) :: * -> *

data Channel (m :: * -> *) (t :: *) where
    NewChannel :: Channel m (ChannelT m t)
    ReadChannel :: ChannelT m t -> Channel m t
    TryReadChannel :: ChannelT m t -> Channel m (Maybe t)
    UnGetChannel :: ChannelT m t -> t -> Channel m ()
    WriteChannel :: ChannelT m t -> t -> Channel m ()

instance (ChannelT n ~ ChannelT m) => MFunctor' Channel m n where
    hoist' _ NewChannel         = NewChannel
    hoist' _ (ReadChannel a)    = ReadChannel a
    hoist' _ (TryReadChannel a) = TryReadChannel a
    hoist' _ (UnGetChannel a b) = UnGetChannel a b
    hoist' _ (WriteChannel a b) = WriteChannel a b

{-# INLINE newChannel #-}
newChannel :: ( Mockable Channel m ) => m (ChannelT m t)
newChannel = liftMockable NewChannel

{-# INLINE readChannel #-}
readChannel :: ( Mockable Channel m ) => ChannelT m t -> m t
readChannel channel = liftMockable $ ReadChannel channel

{-# INLINE tryReadChannel #-}
tryReadChannel :: ( Mockable Channel m ) => ChannelT m t -> m (Maybe t)
tryReadChannel channel = liftMockable $ TryReadChannel channel

{-# INLINE unGetChannel #-}
unGetChannel :: ( Mockable Channel m ) => ChannelT m t -> t -> m ()
unGetChannel channel t = liftMockable $ UnGetChannel channel t

{-# INLINE writeChannel #-}
writeChannel :: ( Mockable Channel m ) => ChannelT m t -> t -> m ()
writeChannel channel t = liftMockable $ WriteChannel channel t
