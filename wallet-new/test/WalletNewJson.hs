module WalletNewJson
       (tests
       ) where

import           Universum

import           Cardano.Wallet.API.V1.Errors (WalletError (..))
import           Cardano.Wallet.API.V1.Types (V1 (..), mkSyncPercentage)
import           Data.List.NonEmpty (fromList)
import           Hedgehog (Property)
import qualified Hedgehog as H
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), Address (..), BlockCount (..),
                     ChainDifficulty (..), Script (..), makeAddress)
import           Pos.Crypto.HD (HDAddressPayload (..))
import           Pos.Wallet.Web (SyncProgress (..))

import           Test.Pos.Util.Golden (discoverGolden, goldenTestJSON)

-------------------------------------------------------------------------------
-- WalletError
-------------------------------------------------------------------------------

golden_WalletError_NotEnoughMoney :: Property
golden_WalletError_NotEnoughMoney =
    goldenTestJSON
        (NotEnoughMoney 10)
            "test/golden/WalletError_NotEnoughMoney"

golden_WalletError_OutputIsRedeem :: Property
golden_WalletError_OutputIsRedeem =
    goldenTestJSON
        exampleAddress
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
        WalletAlreadyExists
            "test/golden/WalletError_WalletAlreadyExists"

golden_WalletError_AddressNotFound :: Property
golden_WalletError_AddressNotFound =
    goldenTestJSON
        AddressNotFound
            "test/golden/WalletError_AddressNotFound"

golden_WalletError_TxFailedToStabilize :: Property
golden_WalletError_TxFailedToStabilize =
    goldenTestJSON
        TxFailedToStabilize
            "test/golden/WalletError_TxFailedToStabilize"

golden_WalletError_TxRedemptionDepleted :: Property
golden_WalletError_TxRedemptionDepleted =
    goldenTestJSON
        TxRedemptionDepleted
            "test/golden/WalletError_TxRedemptionDepleted"

golden_WalletError_TxSafeSignerNotFound :: Property
golden_WalletError_TxSafeSignerNotFound =
    goldenTestJSON
        (TxSafeSignerNotFound exampleAddress)
            "test/golden/WalletError_TxSafeSignerNotFound"

golden_WalletError_MissingRequiredParams :: Property
golden_WalletError_MissingRequiredParams =
    goldenTestJSON
        (MissingRequiredParams (fromList [("test","test")]))
            "test/golden/WalletError_MissingRequiredParams"

golden_WalletError_WalletIsNotReadyToProcessPayments :: Property
golden_WalletError_WalletIsNotReadyToProcessPayments =
    goldenTestJSON
        exampleSyncProgress
            "test/golden/WalletError_WalletIsNotReadyToProcessPayments"

golden_WalletError_NodeIsStillSyncing :: Property
golden_WalletError_NodeIsStillSyncing =
    goldenTestJSON
        (mkSyncPercentage 10)
            "test/golden/WalletError_NodeIsStillSyncing"

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleAddress :: V1 Address
exampleAddress = V1 $ makeAddress (ScriptASD (Script 0 "bytes")) addrAttrib
  where
    addrAttrib =
        AddrAttributes
            (Just $ HDAddressPayload "jpzgcjlmlcetfhrrcgwxqzpfveupoyie")
            BootstrapEraDistr

exampleSyncProgress :: SyncProgress
exampleSyncProgress =
    SyncProgress
            (ChainDifficulty $ BlockCount 64)
            (Just . ChainDifficulty  $ BlockCount 64)
            10

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
