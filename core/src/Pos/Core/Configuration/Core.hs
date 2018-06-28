{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE Rank2Types  #-}

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

       -- * For testing - Should probably be moved from the library
       , defaultCoreConfiguration
       ) where

import           Universum

import           Data.Reflection (Given (..), give)

import qualified Data.HashMap.Strict as HM
import           Pos.Binary.Class (Raw)
import           Pos.Core.Common (Coeff (..), SharedSeed (..), TxFeePolicy (..),
                     TxSizeLinear (..), unsafeCoinPortionFromDouble)
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisInitializer (..),
                     GenesisProtocolConstants (..), GenesisSpec (..),
                     TestnetBalanceOptions (..), noGenesisDelegation)
import           Pos.Core.ProtocolConstants (VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Slotting (EpochIndex (..))
import           Pos.Core.Update (BlockVersionData (..), SoftforkRule (..))
import           Pos.Crypto (ProtocolMagic (..))
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

defaultCoreConfiguration :: CoreConfiguration
defaultCoreConfiguration = CoreConfiguration (GCSpec defaultGenesisSpec)  0

defaultGenesisSpec :: GenesisSpec
defaultGenesisSpec = UnsafeGenesisSpec
  (GenesisAvvmBalances HM.empty)
  (SharedSeed "c2tvdm9yb2RhIEdndXJkYSBib3JvZGEgcHJvdm9kYSA=")
  noGenesisDelegation
  (BlockVersionData
    0
    7000
    2000000
    2000000
    4096
    700
    (unsafeCoinPortionFromDouble 0.01)
    (unsafeCoinPortionFromDouble 0.005)
    (unsafeCoinPortionFromDouble 0.001)
    (unsafeCoinPortionFromDouble 0.1)
    10
    (SoftforkRule (unsafeCoinPortionFromDouble 0.9)
                  (unsafeCoinPortionFromDouble 0.6)
                  (unsafeCoinPortionFromDouble 0.05)
    )
    (TxFeePolicyTxSizeLinear $ TxSizeLinear (Coeff 155381) (Coeff 43.946))
    (EpochIndex maxBound)
  )
  (GenesisProtocolConstants 10
                            (ProtocolMagic 55550001)
                            (VssMaxTTL 6)
                            (VssMinTTL 2)
  )
  (GenesisInitializer (TestnetBalanceOptions 12 4 6e17 0.99 True)
                      (FakeAvvmOptions 10 100000)
                      (unsafeCoinPortionFromDouble 1)
                      True
                      6e17
  )

type HasCoreConfiguration = Given CoreConfiguration

withCoreConfiguration :: CoreConfiguration -> (HasCoreConfiguration => r) -> r
withCoreConfiguration = give

coreConfiguration :: HasCoreConfiguration => CoreConfiguration
coreConfiguration = given

-- | DB format version. When serializing items into the node's DB, the values are paired
-- with this constant.
dbSerializeVersion :: HasCoreConfiguration => Word8
dbSerializeVersion = fromIntegral . ccDbSerializeVersion $ coreConfiguration
