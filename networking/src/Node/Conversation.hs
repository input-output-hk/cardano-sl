{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Node.Conversation
    ( Converse (..)
    , Conversation (..)
    , ConversationActions (..)
    , converseWith
    ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Word (Word32)
import qualified Node.Internal as LL
import           Node.Message.Class

newtype Converse packingType peerData = Converse {
      runConverse :: forall t . LL.NodeId -> (peerData -> Conversation packingType t) -> IO t
    }

converseWith
    :: Converse packingType peerData
    -> LL.NodeId
    -> (peerData -> Conversation packingType t)
    -> IO t
converseWith = runConverse

-- | Use ConversationActions on some Serializable, Message send type, with a
--   Serializable receive type.
data Conversation packingType t where
    Conversation
        :: (Serializable packingType snd, Serializable packingType rcv, Message snd)
        => (ConversationActions snd rcv -> IO t)
        -> Conversation packingType t

data ConversationActions body rcv = ConversationActions {
       -- | Send a message within the context of this conversation
       send :: body -> IO ()

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
       --   The 'Word32' parameter is a limit on how many bytes will be read
       --   in by this use of 'recv'. If the limit is exceeded, the
       --   'LimitExceeded' exception is thrown.
     , recv :: Word32 -> IO (Maybe rcv)

       -- | Send raw bytes.
     , sendRaw :: ByteString -> IO ()
     }
