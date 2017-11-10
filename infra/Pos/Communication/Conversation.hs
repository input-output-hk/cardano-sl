module Pos.Communication.Conversation
    ( ConversationEndpoint (..)
    , ConversationId
    ) where

import           Node.Conversation (ConversationId)
import           Universum

-- | A type represents an endpoint for a conversation.
class ConversationEndpoint ep where
    conversationId :: Proxy ep -> ConversationId
    formatConversationEndpoint :: Proxy ep -> Text
