{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Aeson.Core.Configuration
       (
       ) where

import           Data.Aeson (FromJSON, ToJSON, Value (..), genericToEncoding,
                     pairs, parseJSON, toEncoding, (.:))
import           Data.Aeson.Encoding (pairStr)
import           Data.Aeson.Options (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HMS
import           Data.Monoid ((<>))
import           Pos.Aeson.Core ()
import           Pos.Core.Configuration.Core (CoreConfiguration (..),
                     GenesisConfiguration (..))
import           Pos.Core.Genesis (GenesisAvvmBalances (..),
                     GenesisInitializer (..), GenesisSpec (..))
import           Prelude

instance ToJSON GenesisConfiguration where
    toEncoding (GCSrc gcsFile gcsHash) =
        pairs . pairStr "src"
            . pairs  $ pairStr "file"
                (toEncoding gcsFile) <> pairStr "hash" (toEncoding gcsHash)

    toEncoding (GCSpec value)          =
        genericToEncoding defaultOptions (GCSpec value)

instance FromJSON GenesisConfiguration where
    parseJSON (Object o)
        | HMS.member "src" o  = GCSrc <$> ((o .: "src") >>= (.: "file"))
                                      <*> ((o .: "src") >>= (.: "hash"))
        | HMS.member "spec" o = do
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

deriveJSON defaultOptions ''CoreConfiguration
