{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Node.Conversation
    ( Conversation (..)
    , ConversationActions (..)
    , hoistConversationActions
    , Converse
    ) where

import           Data.Word (Word32)
import qualified Node.Internal as LL
import           Node.Message.Class

type Converse packingType peerData m = forall t .
    LL.NodeId -> (peerData -> Conversation packingType m t) -> m t

-- | Use ConversationActions on some Serializable, Message send type, with a
--   Serializable receive type.
data Conversation packingType m t where
    Conversation
        :: (Serializable packingType snd, Serializable packingType rcv, Message snd)
        => (ConversationActions snd rcv m -> m t)
        -> Conversation packingType m t

data ConversationActions body rcv m = ConversationActions {
       -- | Send a message within the context of this conversation
       send :: body -> m ()

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
       --   The 'Word32' parameter is a limit on how many bytes will be read
       --   in by this use of 'recv'. If the limit is exceeded, the
       --   'LimitExceeded' exception is thrown.
     , recv :: Word32 -> m (Maybe rcv)
     }

hoistConversationActions
    :: (forall a. n a -> m a)
    -> ConversationActions body rcv n
    -> ConversationActions body rcv m
hoistConversationActions nat ConversationActions {..} =
  ConversationActions send' recv'
      where
        send' = nat . send
        recv' = nat . recv
