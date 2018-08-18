module WalletNewJson
       ( tests
       ) where

import           Universum

import           Cardano.Wallet.API.Response (JSONValidationError (..))
import           Cardano.Wallet.API.V1.Migration.Types (MigrationError (..))
import           Cardano.Wallet.API.V1.Types (SyncProgress (..), V1 (..),
                     WalletError (..), exampleWalletId,
                     mkEstimatedCompletionTime, mkSyncPercentage,
                     mkSyncThroughput)
import           Data.List.NonEmpty (fromList)
import           Hedgehog (Property)
import qualified Hedgehog as H
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), Address (..), BlockCount (..),
                     Script (..), makeAddress)
import           Pos.Crypto.HD (HDAddressPayload (..))
import           WalletNewGen (genWalletError)

import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- WalletError
-------------------------------------------------------------------------------

roundTripWalletError :: Property
roundTripWalletError =
    eachOf 5000 genWalletError roundTripsAesonShow

golden_WalletError_NotEnoughMoney :: Property
golden_WalletError_NotEnoughMoney =
    goldenTestJSON
        (NotEnoughMoney 10)
            "test/golden/WalletError_NotEnoughMoney"

golden_WalletError_OutputIsRedeem :: Property
golden_WalletError_OutputIsRedeem =
    goldenTestJSON
        (OutputIsRedeem exampleAddress)
            "test/golden/WalletError_OutputIsRedeem"

golden_WalletError_MigrationFailed :: Property
golden_WalletError_MigrationFailed =
    goldenTestJSON
        (MigrationFailed "test")
            "test/golden/WalletError_MigrationFailed"

golden_WalletError_JSONValidationFailed :: Property
golden_WalletError_JSONValidationFailed =
    goldenTestJSON
        (JSONValidationFailed "test")
            "test/golden/WalletError_JSONValidationFailed"

golden_WalletError_UnknownError :: Property
golden_WalletError_UnknownError =
    goldenTestJSON
        (UnknownError "test")
            "test/golden/WalletError_UnknownError"

golden_WalletError_InvalidAddressFormat :: Property
golden_WalletError_InvalidAddressFormat =
    goldenTestJSON
        (InvalidAddressFormat "test")
            "test/golden/WalletError_InvalidAddressFormat"

golden_WalletError_WalletNotFound :: Property
golden_WalletError_WalletNotFound =
    goldenTestJSON
        WalletNotFound
            "test/golden/WalletError_WalletNotFound"

golden_WalletError_WalletAlreadyExists :: Property
golden_WalletError_WalletAlreadyExists =
    goldenTestJSON
        (WalletAlreadyExists exampleWalletId)
            "test/golden/WalletError_WalletAlreadyExists"

golden_WalletError_AddressNotFound :: Property
golden_WalletError_AddressNotFound =
    goldenTestJSON
        AddressNotFound
