{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : JsonLog.JsonLogT
Description : Monad transformer for JSON logging
License:      MIT
Maintainer:   lars.bruenjes@iohk.io
Stability:    experimental
Portability:  GHC

This module provides the monad transformer @'JsonLogT'@
for adding JSON logging to a monad transformer stack.
-}

module Pos.Core.JsonLog.JsonLogT
    ( JsonLogT
    , runWithoutJsonLogT
    , runJsonLogT
    , runJsonLogT'
    , runWithJsonLogT
    , runWithJsonLogT'
    , JsonLogConfig(..)
    , jsonLogDefault
    ) where

import           Universum

import           Control.Concurrent.MVar (MVar, withMVar)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (MFunctor (..))
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Trans.Lift.Local (LiftLocal)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Aeson (ToJSON, encode)
import           Data.ByteString.Lazy.Char8 (hPutStrLn)
import           System.IO (Handle, hFlush)

import           Pos.Core.JsonLog.CanJsonLog (CanJsonLog (..))
import           Pos.Core.JsonLog.Event (JLTimedEvent, timedIO, toEvent)

data JsonLogConfig
    = JsonLogDisabled
    | JsonLogConfig (MVar Handle) (JLTimedEvent -> IO Bool)

-- | Monad transformer @'JsonLogT'@ adds support for JSON logging
-- to a monad transformer stack.
newtype JsonLogT m a = JsonLogT (ReaderT JsonLogConfig m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MFunctor,
              MonadThrow, MonadCatch, MonadMask, MonadFix, MonadBase b, LiftLocal)

instance MonadBaseControl b m => MonadBaseControl b (JsonLogT m) where

    type StM (JsonLogT m) a = StM m a

    liftBaseWith f = liftBaseWith f

    restoreM = restoreM

jsonLogDefault
    :: (ToJSON a, MonadIO m, MonadCatch m)
    => JsonLogConfig
    -> a
    -> m ()
jsonLogDefault jlc x =
    case jlc of
        JsonLogDisabled -> return ()
        JsonLogConfig v decide -> do
            event <- toEvent <$> timedIO x
            b     <- liftIO (decide event)
                `catchAny` \_ -> do
                    --logWarning logTrace $ sformat ("error in deciding whether to json log: "%shown) e
                    return False
            when b $ liftIO (withMVar v $ \h -> (hPutStrLn h (encode event) >> hFlush h))
                `catchAny` \_ -> return ()
                    --logWarning logTrace $ sformat ("can't write json log: "%shown) e

instance ( MonadIO m
         , MonadCatch m) => CanJsonLog (JsonLogT m) where

    jsonLog x = JsonLogT (ReaderT $ \jlc -> jsonLogDefault jlc x)   -- TODO

-- | This function simply discards all JSON log messages.
runWithoutJsonLogT :: JsonLogT m a -> m a
runWithoutJsonLogT (JsonLogT m) = runReaderT m JsonLogDisabled

-- | Runs a computation containing JSON log messages,
-- either discarding all messages or writing
-- some of them to a handle.
runJsonLogT :: MonadIO m
            => Maybe (Handle, JLTimedEvent -> IO Bool) -- ^ If @'Nothing'@, JSON log messages are discarded, if @'Just' (h, f)@,
                                                       -- log messages @e@ are written to handle @h@ if @f e@ returns @True@,
                                                       -- and are otherwise discarded.
            -> JsonLogT m a                            -- ^ A monadic computation containing JSON log messages.
            -> m a
runJsonLogT Nothing            m            = runWithoutJsonLogT m
runJsonLogT (Just (h, decide)) (JsonLogT m) = do
    v <- newMVar h
    runReaderT m $ JsonLogConfig v decide

-- | Runs a computation containing JSON log messages,
-- either discarding all messages or writing them to a handle.
runJsonLogT' :: MonadIO m
             => Maybe Handle -- ^ If @'Nothing'@, JSON log messages are discarded, if @'Just' h@,
                             -- log messages are written to handle @h@.
             -> JsonLogT m a -- ^ A monadic computation containing JSON log messages.
             -> m a
runJsonLogT' mh = runJsonLogT $ fmap (, const $ return True) mh

-- | Runs a computation containing JSON log messages,
-- writing some of them to a handle.
runWithJsonLogT :: MonadIO m
                => Handle                    -- ^ The handle to write log messages to.
                -> (JLTimedEvent -> IO Bool) -- ^ Monadic predicate to decide whether a given log message
                                             -- should be written to the handle or be discarded.
                -> JsonLogT m a              -- ^ A monadic computation containing JSON log messages.
                -> m a
runWithJsonLogT h decide = runJsonLogT $ Just (h, decide)

-- | Runs a computation containing JSON log messages,
-- writing them to a handle.
runWithJsonLogT' :: MonadIO m
                 => Handle       -- ^ The handle to write log messages to.
                 -> JsonLogT m a -- ^ A monadic computation containing JSON log messages.
                 -> m a
runWithJsonLogT' = runJsonLogT' . Just
