{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Pos.Ssc.Class.Types
       ( Ssc(..)
       ) where

import           Data.Binary         (Binary)
import           Data.SafeCopy       (SafeCopy)
import           Data.Tagged         (Tagged)
import           Data.Text.Buildable (Buildable)
import           Universum

class (Typeable ssc
      ,Typeable (SscToken ssc)
      ,Typeable (SscPayload ssc)
      ,Typeable (SscStorage ssc)
      ,Typeable (SscProof ssc)
      ,Typeable (SscMessage ssc)
      ,Typeable (SscSeedError ssc)
      ,Eq (SscProof ssc)
      ,Show (SscProof ssc)
      ,Show (SscPayload ssc)
      ,Buildable (SscPayload ssc)
      ,Buildable (SscSeedError ssc)
      ,Binary (SscProof ssc)
      ,Binary (SscPayload ssc)
      ,Binary (SscMessage ssc)
      ,SafeCopy (SscProof ssc)
      ,SafeCopy (SscPayload ssc)
      ,SafeCopy (SscMessage ssc)
      ,SafeCopy (SscToken ssc)
      ,SafeCopy (SscStorage ssc)) =>
      Ssc ssc where

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
    -- | This is BARDAQ needed only for dynamic state implementation.
    type SscToken ssc

    -- | Create proof (for inclusion into block header) from payload
    mkSscProof :: Tagged ssc (SscPayload ssc -> SscProof ssc)
