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

import           Prelude
import           Universum hiding (fail, (<>))

import           Data.Aeson (FromJSON, ToJSON, Value (..), genericToEncoding,
                     pairs, parseJSON, toEncoding, (.:))
import           Data.Aeson.Encoding (pairStr)
import           Data.Aeson.Options (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.Reflection (Given (..), give)

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
    | GCSrc !FilePath !(Hash Raw)
      -- !FilePath = Path to file where 'GenesisData' is stored. Must be
      -- in JSON, not necessary canonical.
      -- !(Hash Raw) = Hash of canonically encoded 'GenesisData'.
    deriving (Eq, Show, Generic)

instance ToJSON GenesisConfiguration where
    toEncoding (GCSrc gcsFile gcsHash) =
        pairs . pairStr "src"
            . pairs  $ pairStr "file"
                (toEncoding gcsFile) <> pairStr "hash" (toEncoding gcsHash)

    toEncoding (GCSpec value)          =
        genericToEncoding defaultOptions (GCSpec value)

instance FromJSON GenesisConfiguration where
    parseJSON (Object o)
        | HM.member "src" o  = GCSrc <$> ((o .: "src") >>= (.: "file"))
                                      <*> ((o .: "src") >>= (.: "hash"))
        | HM.member "spec" o = do
              -- GCSpec Object
              specO <- o .: "spec"

              -- GenesisAvvmBalances
              avvmDistrO <- specO .: "avvmDistr"
              avvmDistr <- parseJSON (avvmDistrO)

              -- SharedSeed
              ftsSeed <- specO .: "ftsSeed"

              -- GenesisDelegation
              heavyDelegationO <- specO .: "heavyDelegation"
              heavyDelegation <- parseJSON (heavyDelegationO)

              -- BlockVersionData
              blockVersionDataO <- specO .: "blockVersionData"
              blockVersionData <- parseJSON blockVersionDataO

              -- GenesisProtocolConstants
              protocolConstantsO <- specO .: "protocolConstants"
              protocolConstants <- parseJSON protocolConstantsO

              -- GenesisInitializer
              initializerO <- specO .: "initializer"
              testBalanceO <- initializerO .: "testBalance"
              testBalance <- parseJSON testBalanceO
              fakeAvvmBalanceO <- (initializerO .: "fakeAvvmBalance")
              fakeAvvmBalance <- parseJSON fakeAvvmBalanceO
              avvmBalanceFactor <- initializerO .: "avvmBalanceFactor"
              useHeavyDlg <- initializerO .: "useHeavyDlg"
              seed <- initializerO .: "seed"

              return . GCSpec $
                  UnsafeGenesisSpec
                      (GenesisAvvmBalances avvmDistr)
                      ftsSeed
                      heavyDelegation
                      blockVersionData
                      protocolConstants
                      (GenesisInitializer
                          testBalance
                          fakeAvvmBalance
                          avvmBalanceFactor
                          useHeavyDlg
                          seed)
        | otherwise = fail "Incorrect JSON encoding for GenesisConfiguration"

    parseJSON invalid = typeMismatch "GenesisConfiguration" invalid

data CoreConfiguration = CoreConfiguration
    {
      -- | Specifies the genesis
      ccGenesis            :: !GenesisConfiguration

      -- | Versioning for values in node's DB
    , ccDbSerializeVersion :: !Word8

    }
    deriving (Show, Generic)

deriveJSON defaultOptions ''CoreConfiguration

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
