{-# LANGUAGE AllowAmbiguousTypes #-}

module Pos.Ssc.Class.Helpers
       ( SscHelpersClass (..)
       ) where

import           Universum

import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core                   (BlockCount, EpochIndex, IsMainHeader,
                                             LocalSlotIndex)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Util.Util              (Some)

class Ssc ssc => SscHelpersClass ssc where
    -- | Verify that payload can be included into a block for the
    -- given epoch.
    sscVerifyPayloadEpoch
        :: EpochIndex
        -> SscPayload ssc
        -> Either (SscVerifyError ssc) ()
    -- | Removes parts of payload so its binary representation length
    -- fits into passed limit. If limit is too low (0), we can return
    -- 'Nothing'.
    sscStripPayload :: Byte -> SscPayload ssc -> Maybe (SscPayload ssc)
    -- | Returns default payload for the given local slot index.
    sscDefaultPayload :: BlockCount -> LocalSlotIndex -> (SscPayload ssc)
