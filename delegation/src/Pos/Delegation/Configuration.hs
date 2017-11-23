{-# LANGUAGE RankNTypes #-}

-- | Configuration part of delegation.

module Pos.Delegation.Configuration
    ( HasDlgConfiguration
    , withDlgConfiguration
    , dlgConfiguration
    , DlgConfiguration(..)
    , dlgCacheParam
    , dlgMessageCacheTimeout
    ) where

import           Universum

import           Data.Aeson (FromJSON (..), genericParseJSON)
import           Data.Reflection (Given (..), give)
import           Serokell.Aeson.Options (defaultOptions)


type HasDlgConfiguration = Given DlgConfiguration

withDlgConfiguration :: DlgConfiguration -> (HasDlgConfiguration => r) -> r
withDlgConfiguration = give

dlgConfiguration :: HasDlgConfiguration => DlgConfiguration
dlgConfiguration = given

-- | Delegation configruation part.
data DlgConfiguration = DlgConfiguration
    {
      ccDlgCacheParam       :: !Int
      -- ^ This value parameterizes size of cache used in Delegation.
      -- Not bytes, but number of elements.
    , ccMessageCacheTimeout :: !Int
      -- ^ Interval we ignore cached messages for if it's sent again.
    } deriving (Eq,Show,Generic)

instance FromJSON DlgConfiguration where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | This value parameterizes size of cache used in Delegation.
-- Not bytes, but number of elements.
dlgCacheParam :: (HasDlgConfiguration, Integral n) => n
dlgCacheParam = fromIntegral . ccDlgCacheParam $ dlgConfiguration

-- | Timeout for caching system. Delegation uses this timeout to
-- invalidate caches. Later it will be (hopefully) eliminated and
-- replaced by the diffusion layer.
dlgMessageCacheTimeout :: (HasDlgConfiguration, Integral a) => a
dlgMessageCacheTimeout = fromIntegral . ccMessageCacheTimeout $ dlgConfiguration
