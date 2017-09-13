module Pos.Communication.Conversation
    ( ConversationEndpoint (..)
    , ConversationId
    ) where

import           Universum
import           Node.Conversation (ConversationId)

-- | A type represents an endpoint for a conversation.
class ConversationEndpoint ep where
    conversationId :: Proxy ep -> ConversationId
    formatConversationEndpoint :: Proxy ep -> Text
