{-# LANGUAGE AllowAmbiguousTypes #-}

module Pos.Ssc.Class.Helpers
       ( SscHelpersClass (..)
       ) where

import           Universum

import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core                   (EpochIndex, IsMainHeader, LocalSlotIndex)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Util.Util              (Some)

class Ssc ssc => SscHelpersClass ssc where
    sscVerifyPayload
        :: Either EpochIndex (Some IsMainHeader)
        -> SscPayload ssc
        -> Either (SscVerifyError ssc) ()
    -- | Removes parts of payload so its binary representation length
    -- fits into passed limit. If limit is too low (0), we can return
    -- 'Nothing'.
    sscStripPayload :: Byte -> SscPayload ssc -> Maybe (SscPayload ssc)
    -- | Returns default payload for the given local slot index.
    sscDefaultPayload :: LocalSlotIndex -> (SscPayload ssc)
