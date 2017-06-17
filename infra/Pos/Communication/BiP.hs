-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP(..)
       ) where

import           Universum

import           Node.Message.Class            (Serializable (..))
import           Node.Message.Binary           (binaryPackMsg, binaryUnpackMsg)

import           Pos.Binary.Class              (Bi (..))

data BiP = BiP

instance Bi r => Serializable BiP r where
    packMsg _ = binaryPackMsg . put
    unpackMsg _ = binaryUnpackMsg get
