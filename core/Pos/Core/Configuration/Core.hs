{-# LANGUAGE Rank2Types #-}

-- | Global constants, configurable via Data.Reflection.

module Pos.Core.Configuration.Core
       (
       -- * The configuration structure
         CoreConfiguration(..)
       , GenesisConfiguration(..)

       , HasCoreConfiguration
       , withCoreConfiguration

       , coreConfiguration
       , dbSerializeVersion
       ) where

import           Universum

import           Data.Reflection (Given (..), give)

import           Pos.Binary.Class (Raw)
import           Pos.Core.Genesis.Types (GenesisSpec (..))
import           Pos.Crypto.Hashing (Hash)

data GenesisConfiguration
      -- | Genesis from a 'GenesisSpec'.
    = GCSpec !GenesisSpec
      -- | 'GenesisData' is stored in a file.
    | GCSrc { gcsFile :: !FilePath
            -- ^ Path to file where 'GenesisData' is stored. Must be
            -- in JSON, not necessary canonical.
            , gcsHash :: !(Hash Raw)
            -- ^ Hash of canonically encoded 'GenesisData'.
            }
    deriving (Show)

data CoreConfiguration = CoreConfiguration
    {
      -- | Specifies the genesis
      ccGenesis            :: !GenesisConfiguration

      -- | Versioning for values in node's DB
    , ccDbSerializeVersion :: !Word8

    }
    deriving (Show, Generic)

type HasCoreConfiguration = Given CoreConfiguration

withCoreConfiguration :: CoreConfiguration -> (HasCoreConfiguration => r) -> r
withCoreConfiguration = give

coreConfiguration :: HasCoreConfiguration => CoreConfiguration
coreConfiguration = given

-- | DB format version. When serializing items into the node's DB, the values are paired
-- with this constant.
dbSerializeVersion :: HasCoreConfiguration => Word8
dbSerializeVersion = fromIntegral . ccDbSerializeVersion $ coreConfiguration
