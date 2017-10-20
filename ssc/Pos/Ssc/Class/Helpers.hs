{-# LANGUAGE AllowAmbiguousTypes #-}

module Pos.Ssc.Class.Helpers
       ( SscHelpersClass (..)
       ) where

import           Universum

import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core                   (EpochIndex, IsMainHeader, LocalSlotIndex)
import           Pos.Ssc.Core.Types         (SscPayload)
import           Pos.Ssc.VerifyError        (SscVerifyError)
import           Pos.Util.Util              (Some)

class SscHelpersClass where
    sscVerifyPayload
        :: Either EpochIndex (Some IsMainHeader)
        -> SscPayload
        -> Either SscVerifyError ()
    -- | Removes parts of payload so its binary representation length
    -- fits into passed limit. If limit is too low (0), we can return
    -- 'Nothing'.
    sscStripPayload :: Byte -> SscPayload -> Maybe SscPayload
    -- | Returns default payload for the given local slot index.
    sscDefaultPayload :: LocalSlotIndex -> SscPayload
    -- | Returns 'True' if the error must be reported.
    sscIsCriticalError :: SscVerifyError -> Bool
