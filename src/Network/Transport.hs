{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Network Transport
module Network.Transport
  ( -- * Types
    Transport(..)
  , EndPoint(..)
  , Connection(..)
  , Event(..)
  , ConnectionId
  , Reliability(..)
  , EndPointAddress(..)
    -- * Hints
  , ConnectHints(..)
  , defaultConnectHints
    -- * Error codes
  , TransportError(..)
  , NewEndPointErrorCode(..)
  , ConnectErrorCode(..)
  , SendErrorCode(..)
  , EventErrorCode(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (copy)
import qualified Data.ByteString.Char8 as BSC (unpack)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Binary (Binary(..))
import Data.Hashable
import Data.Word (Word64)
import Data.Data (Data)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Main API                                                                   --
--------------------------------------------------------------------------------

-- | A network transport over some monad.
data Transport m = Transport {
    -- | Create a new end point (heavyweight operation)
    newEndPoint :: m (Either (TransportError NewEndPointErrorCode) (EndPoint m))
    -- | Shutdown the transport completely
  , closeTransport :: m ()
  }

-- | Network endpoint over some monad.
data EndPoint m = EndPoint {
    -- | Endpoints have a single shared receive queue.
    receive :: m Event
    -- | EndPointAddress of the endpoint.
  , address :: EndPointAddress
    -- | Create a new lightweight connection.
    --
    -- 'connect' should be as asynchronous as possible; for instance, in
    -- Transport implementations based on some heavy-weight underlying network
    -- protocol (TCP, ssh), a call to 'connect' should be asynchronous when a
    -- heavyweight connection has already been established.
  , connect :: EndPointAddress -> Reliability -> ConnectHints -> m (Either (TransportError ConnectErrorCode) (Connection m))
    -- | Close the endpoint
  , closeEndPoint :: m ()
  }

-- | Lightweight connection to an endpoint.
data Connection m = Connection {
    -- | Send a message on this connection.
    --
    -- 'send' provides vectored I/O, and allows multiple data segments to be
    -- sent using a single call (cf. 'Network.Socket.ByteString.sendMany').
    -- Note that this segment structure is entirely unrelated to the segment
    -- structure /returned/ by a 'Received' event.
    send :: [ByteString] -> m (Either (TransportError SendErrorCode) ())
    -- | Close the connection.
  , close :: m ()
  }

-- | Event on an endpoint.
data Event =
    -- | Received a message
    Received {-# UNPACK #-} !ConnectionId [ByteString]
    -- | Connection closed
  | ConnectionClosed {-# UNPACK #-} !ConnectionId
    -- | Connection opened
    --
    -- 'ConnectionId's need not be allocated contiguously.
  | ConnectionOpened {-# UNPACK #-} !ConnectionId Reliability EndPointAddress
    -- | Received multicast
    -- | The endpoint got closed (manually, by a call to closeEndPoint or closeTransport)
  | EndPointClosed
    -- | An error occurred
  | ErrorEvent (TransportError EventErrorCode)
  deriving (Show, Eq, Generic)

instance Binary Event

-- | Connection data ConnectHintsIDs enable receivers to distinguish one connection from another.
type ConnectionId = Word64

-- | Reliability guarantees of a connection.
data Reliability =
    ReliableOrdered
  | ReliableUnordered
  | Unreliable
  deriving (Show, Eq, Typeable, Generic)

instance Binary Reliability

-- | EndPointAddress of an endpoint.
newtype EndPointAddress = EndPointAddress { endPointAddressToByteString :: ByteString }
  deriving (Eq, Ord, Typeable, Data, Hashable)

instance Binary EndPointAddress where
  put = put . endPointAddressToByteString
  get = EndPointAddress . BS.copy <$> get

instance Show EndPointAddress where
  show = BSC.unpack . endPointAddressToByteString

instance NFData EndPointAddress where rnf x = x `seq` ()

--------------------------------------------------------------------------------
-- Hints                                                                      --
--                                                                            --
-- Hints provide transport-generic "suggestions". For now, these are          --
-- placeholders only.                                                         --
--------------------------------------------------------------------------------

-- | Hints used by 'connect'
data ConnectHints = ConnectHints {
    -- Timeout
    connectTimeout :: Maybe Int
  }

-- | Default hints for connecting
defaultConnectHints :: ConnectHints
defaultConnectHints = ConnectHints {
    connectTimeout = Nothing
  }

--------------------------------------------------------------------------------
-- Error codes                                                                --
--                                                                            --
-- Errors should be transport-implementation independent. The deciding factor --
-- for distinguishing one kind of error from another should be: might         --
-- application code have to take a different action depending on the kind of  --
-- error?                                                                     --
--------------------------------------------------------------------------------

-- | Errors returned by Network.Transport API functions consist of an error
-- code and a human readable description of the problem
data TransportError error = TransportError error String
  deriving (Show, Typeable, Generic)

instance (Binary error) => Binary (TransportError error)

-- | Although the functions in the transport API never throw TransportErrors
-- (but return them explicitly), application code may want to turn these into
-- exceptions.
instance (Typeable err, Show err) => Exception (TransportError err)

-- | When comparing errors we ignore the human-readable strings
instance Eq error => Eq (TransportError error) where
  TransportError err1 _ == TransportError err2 _ = err1 == err2

-- | Errors during the creation of an endpoint
data NewEndPointErrorCode =
    -- | Not enough resources
    NewEndPointInsufficientResources
    -- | Failed for some other reason
  | NewEndPointFailed
  deriving (Show, Typeable, Eq)

-- | Connection failure
data ConnectErrorCode =
    -- | Could not resolve the address
    ConnectNotFound
    -- | Insufficient resources (for instance, no more sockets available)
  | ConnectInsufficientResources
    -- | Timeout
  | ConnectTimeout
    -- | Failed for other reasons (including syntax error)
  | ConnectFailed
  deriving (Show, Typeable, Eq)

-- | Failure during sending a message
data SendErrorCode =
    -- | Connection was closed
    SendClosed
    -- | Send failed for some other reason
  | SendFailed
  deriving (Show, Typeable, Eq)

-- | Error codes used when reporting errors to endpoints (through receive)
data EventErrorCode =
    -- | Failure of the entire endpoint
    EventEndPointFailed
    -- | Transport-wide fatal error
  | EventTransportFailed
    -- | We lost connection to another endpoint
    --
    -- Although "Network.Transport" provides multiple independent lightweight
    -- connections between endpoints, those connections cannot /fail/
    -- independently: once one connection has failed, /all/ connections, in
    -- both directions, must now be considered to have failed; they fail as a
    -- "bundle" of connections, with only a single "bundle" of connections per
    -- endpoint at any point in time.
    --
    -- That is, suppose there are multiple connections in either direction
    -- between endpoints A and B, and A receives a notification that it has
    -- lost contact with B. Then A must not be able to send any further
    -- messages to B on existing connections.
    --
    -- Although B may not realize /immediately/ that its connection to A has
    -- been broken, messages sent by B on existing connections should not be
    -- delivered, and B must eventually get an EventConnectionLost message,
    -- too.
    --
    -- Moreover, this event must be posted before A has successfully
    -- reconnected (in other words, if B notices a reconnection attempt from A,
    -- it must post the EventConnectionLost before acknowledging the connection
    -- from A) so that B will not receive events about new connections or
    -- incoming messages from A without realizing that it got disconnected.
    --
    -- If B attempts to establish another connection to A before it realized
    -- that it got disconnected from A then it's okay for this connection
    -- attempt to fail, and the EventConnectionLost to be posted at that point,
    -- or for the EventConnectionLost to be posted and for the new connection
    -- to be considered the first connection of the "new bundle".
  | EventConnectionLost EndPointAddress
  deriving (Show, Typeable, Eq, Generic)

instance Binary EventErrorCode
