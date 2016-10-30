{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Pos.Ssc.Class.Types
       ( SscTypes(..)
       ) where

import           Data.Binary      (Binary)
import           Data.MessagePack (MessagePack)
import           Data.SafeCopy    (SafeCopy)
import           Data.Tagged      (Tagged)
import           Universum

class (Typeable a
      ,Eq (SscProof a)
      ,Show (SscProof a)
      ,Binary (SscProof a)
      ,Binary (SscPayload a)
      ,MessagePack (SscProof a)
      ,MessagePack (SscPayload a)
      ,SafeCopy (SscProof a)
      ,SafeCopy (SscPayload a)) =>
      SscTypes a where

    -- | Internal SSC state
    type SscInternalState a
    -- | Payload which will be stored in main blocks
    type SscPayload a
    -- | Proof that SSC payload is correct (it'll be included into block
    -- header)
    type SscProof a
    -- | Messages that nodes send to each other to achieve SSC (this
    -- is going to be an ADT if there are many possible messages)
    type SscMessage a
    -- | Error that can happen when calculating the seed
    type SscSeedError a

    -- | Create payload (for inclusion into block) from state
    mkSscPayload :: Tagged a (SscInternalState a -> SscPayload a)
    -- | Create proof (for inclusion into block header) from payload
    mkSscProof :: Tagged a (SscPayload a -> SscProof a)
