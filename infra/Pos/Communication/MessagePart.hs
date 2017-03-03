module Pos.Communication.MessagePart
       ( MessagePart (..)
       ) where

import           Node.Message (MessageName (..))
import           Universum

class MessagePart a where
    pMessageName :: Proxy a -> MessageName
