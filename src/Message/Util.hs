{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Message.Util
    ( sourceChannel
    , fuseChannel
    ) where

import           Control.Monad         (forM_)
import           Control.Monad.Trans   (lift)
import           Data.Conduit          (Conduit, Sink, Source, await, awaitForever,
                                        fuseReturnLeftovers, leftover, yield, ($$), (=$=))
import qualified Data.Conduit.List     as CL
import           Data.Functor.Identity (runIdentity)
import           Data.String           (IsString)
import           GHC.Generics          (Generic)
import           Mockable.Channel      (Channel, ChannelT, readChannel, unGetChannel,
                                        writeChannel)
import           Mockable.Class        (Mockable)

-- | Converts channel to source
sourceChannel :: ( Show a, Mockable Channel m )
              => ChannelT m (Maybe a)
              -> Source m a
sourceChannel chan =
    loop
  where
    loop = do
        mx <- lift $ readChannel chan
        forM_ mx $ \x -> yield x >> loop

-- | Fuses channel with `Sink`, so that sink consumes exactly required amount of data
-- from channel, accounting leftovers.
fuseChannel :: ( Show a, Mockable Channel m )
              => ChannelT m (Maybe a)
              -> Sink a m r
              -> m r
fuseChannel chan sink = do
    (res, leftovers) <-
        sourceChannel chan $$ awaitForever yield `fuseReturnLeftovers` sink

    -- NOTE: `reverse` usage here is not tested, it may be incorrect
    mapM_ (unGetChannel chan . Just) $ reverse leftovers
    pure res
