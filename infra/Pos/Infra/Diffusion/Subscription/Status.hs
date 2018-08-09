{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Infra.Diffusion.Subscription.Status
    ( SubscriptionStatus (..)
    , SubscriptionStates (..)
    , emptySubscriptionStates
    , subscribing
    , subscribed
    , terminated

    , Changes (..)
    , changes
    , withChanges
    ) where

import           Prelude

import           Control.Concurrent.STM (TVar, atomically, newTVarIO, retry, readTVar, writeTVar)
import           Control.Monad (when)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

data SubscriptionStatus =
      Subscribed
    | Subscribing
    deriving (Eq, Ord, Show, Generic)

-- | Mutable 'SubscriptionStatus'es keyed on some type.
newtype SubscriptionStates peer = SubscriptionStates
    { ssMap :: TVar (Map peer SubscriptionStatus)
    }

emptySubscriptionStates :: IO (SubscriptionStates peer)
emptySubscriptionStates = SubscriptionStates <$> newTVarIO Map.empty

subscribing :: Ord peer => SubscriptionStates peer -> peer -> IO ()
subscribing = alterStatus (Just Subscribing)

subscribed :: Ord peer => SubscriptionStates peer -> peer -> IO ()
subscribed = alterStatus (Just Subscribed)

terminated :: Ord peer => SubscriptionStates peer -> peer -> IO ()
terminated = alterStatus Nothing

alterStatus :: Ord peer => Maybe SubscriptionStatus -> SubscriptionStates peer -> peer -> IO ()
alterStatus val (SubscriptionStates tvar) key = atomically $ do
    states <- readTVar tvar
    let !states' = Map.alter (const val) key states
    writeTVar tvar states'

newtype Changes peer = Changes
    { getChanges :: IO (Update peer)
    }

data Update peer = Update
    { newStatuses :: !(Map peer SubscriptionStatus)
    , nextUpdate  :: !(Changes peer)
    }

changes :: forall peer . Ord peer => SubscriptionStates peer -> Changes peer
changes = changes_ Map.empty
  where
    changes_ :: Map peer SubscriptionStatus -> SubscriptionStates peer -> Changes peer
    changes_ current ss@(SubscriptionStates tvar) = Changes $ do
        current' <- atomically $ do
            next <- readTVar tvar
            when (current == next) retry
            pure next
        pure $ Update
            { newStatuses = current'
            , nextUpdate = changes_ current' ss
            }

-- | The changes to a 'SubscriptionStates'.
-- `getChanges` will return when the 'SubscriptionStates' has changed
-- Which changes are actually observed depends upon the runtime system.
-- Many mutations could happen concurrently while another thread uses this
-- 'Changes' to try and get an update
withChanges
    :: Changes peer
    -> (s -> Map peer SubscriptionStatus -> IO (Either s t))
    -> s
    -> IO (s, t)
withChanges (Changes next) k s = do
    upd <- next
    outcome <- k s (newStatuses upd)
    case outcome of
        Left s' -> withChanges (nextUpdate upd) k s'
        Right t -> pure (s, t)
