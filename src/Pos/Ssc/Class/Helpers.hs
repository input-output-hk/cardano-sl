{-# LANGUAGE AllowAmbiguousTypes #-}

module Pos.Ssc.Class.Helpers
       ( SscHelpersClass (..)
       ) where

import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Core.Types             (EpochIndex)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Types.Block.Types      (MainBlockHeader)

class Ssc ssc => SscHelpersClass ssc where
    sscVerifyPayload
        :: Either EpochIndex (MainBlockHeader ssc)
        -> SscPayload ssc
        -> Either (SscVerifyError ssc) ()
    -- | Removes parts of payload so its binary representation length
    -- fits into passed limit. If limit is too low (0), we can return
    -- 'Nothing'.
    sscStripPayload :: Byte -> SscPayload ssc -> Maybe (SscPayload ssc)
