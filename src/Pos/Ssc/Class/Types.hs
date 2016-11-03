{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Pos.Ssc.Class.Types
       ( SscTypes(..)
       ) where

import           Data.Binary         (Binary)
import           Data.SafeCopy       (SafeCopy)
import           Data.Tagged         (Tagged)
import           Data.Text.Buildable (Buildable)
import           Universum

-- TODO: rename to Ssc?
class (Typeable ssc
      ,Eq (SscProof ssc)
      ,Show (SscProof ssc)
      ,Show (SscPayload ssc)
      ,Buildable (SscPayload ssc)
      ,Binary (SscProof ssc)
      ,Binary (SscPayload ssc)
      ,SafeCopy (SscProof ssc)
      ,SafeCopy (SscPayload ssc)) =>
      SscTypes ssc where

    -- | Internal SSC state
    type SscStorage ssc
    -- | Payload which will be stored in main blocks
    type SscPayload ssc
    -- | Proof that SSC payload is correct (it'll be included into block
    -- header)
    type SscProof ssc
    -- | Messages that nodes send to each other to achieve SSC (this
    -- is going to be an ADT if there are many possible messages)
    type SscMessage ssc
    -- | Error that can happen when calculating the seed
    type SscSeedError ssc

    -- | Create payload (for inclusion into block) from state
    mkSscPayload :: Tagged ssc (SscStorage ssc -> SscPayload ssc)
    -- | Create proof (for inclusion into block header) from payload
    mkSscProof :: Tagged ssc (SscPayload ssc -> SscProof ssc)
