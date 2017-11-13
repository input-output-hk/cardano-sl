-- | Indexed connections set

module Pos.Wallet.Web.Sockets.ConnSet
    ( ConnectionSet
    , ConnectionTag
    , ConnectionsVar

    , registerConnection
    , deregisterConnection
    , listConnections
    ) where

import           Universum

import           Control.Monad.State.Strict (MonadState (get, put))
import           Data.Default (Default (..))
import qualified Data.IntMap.Strict as IM
import           Serokell.Util.Concurrent (modifyTVarS)

import           Pos.Wallet.Web.Sockets.Types (WSConnection)

data TaggedSet v = TaggedSet
    { tsUnusedTag :: IM.Key
    , tsData      :: IntMap v
    }

instance Default (TaggedSet v) where
  def = TaggedSet 0 IM.empty

type ConnectionTag = IM.Key
type ConnectionSet = TaggedSet WSConnection
type ConnectionsVar = TVar ConnectionSet

registerConnection :: WSConnection -> ConnectionsVar -> STM ConnectionTag
registerConnection conn var =
    modifyTVarS var $ state $ registerConnectionPure conn

deregisterConnection :: ConnectionTag -> ConnectionsVar -> STM (Maybe WSConnection)
deregisterConnection tag var = modifyTVarS var $ do
    conns <- get
    case deregisterConnectionPure tag conns of
        Nothing -> pure Nothing
        Just (conn, newConns) -> do
            put newConns
            pure (Just conn)

registerConnectionPure :: WSConnection -> ConnectionSet -> (ConnectionTag, ConnectionSet)
registerConnectionPure conn conns =
  let newKey = tsUnusedTag conns + 1 in
  (newKey, TaggedSet newKey (IM.insert newKey conn $ tsData conns))

deregisterConnectionPure :: ConnectionTag -> ConnectionSet -> Maybe (WSConnection, ConnectionSet)
deregisterConnectionPure tag conns = do  -- Maybe monad
  conn <- IM.lookup tag (tsData conns)
  pure (conn, TaggedSet (tsUnusedTag conns) (IM.delete tag $ tsData conns))

listConnections :: ConnectionSet -> [WSConnection]
listConnections = IM.elems . tsData
