module Pos.Communication.MessagePart
       ( MessagePart (..)
       ) where

import           Data.Tagged  (Tagged)
import           Node.Message (MessageName (..))
import           Universum

class MessagePart a where
    pMessageName :: Proxy a -> MessageName

instance MessagePart s => MessagePart (Tagged s a) where
    pMessageName p = pMessageName $
        (const Proxy :: Proxy (Tagged s a) -> Proxy s) p
