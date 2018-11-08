module WalletNewJson
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)

import           Cardano.Wallet.API.Response (JSONValidationError (..),
                     UnsupportedMimeTypeError (..))
import           Cardano.Wallet.API.V1.Swagger.Example (genExample)
import           Cardano.Wallet.API.V1.Types (ErrNotEnoughMoney (..), V1 (..),
                     WalletError (..), exampleWalletId)

import           Test.Pos.Core.ExampleHelpers (exampleAddress)
import           Test.Pos.Util.Golden (discoverGolden, goldenTestJSON)

import qualified Hedgehog as H

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests =
    H.checkSequential $$discoverGolden


-------------------------------------------------------------------------------
-- WalletError
-------------------------------------------------------------------------------

golden_WalletError_NotEnoughMoneyAvailableBalanceIsInsufficient :: Property
golden_WalletError_NotEnoughMoneyAvailableBalanceIsInsufficient =
    goldenTestJSON
        (NotEnoughMoney (ErrAvailableBalanceIsInsufficient 14))
            "test/golden/WalletError_NotEnoughMoneyAvailableBalanceIsInsufficient"

golden_WalletError_NotEnoughMoneyCannotCoverFee :: Property
golden_WalletError_NotEnoughMoneyCannotCoverFee =
    goldenTestJSON
        (NotEnoughMoney ErrCannotCoverFee)
            "test/golden/WalletError_NotEnoughMoneyCannotCoverFee"


golden_WalletError_OutputIsRedeem :: Property
golden_WalletError_OutputIsRedeem =
    goldenTestJSON
        (OutputIsRedeem $ V1 exampleAddress)
            "test/golden/WalletError_OutputIsRedeem"

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
            "test/golden/WalletError_AddressNotFound"

golden_WalletError_InvalidPublicKey :: Property
golden_WalletError_InvalidPublicKey =
    goldenTestJSON
        (InvalidPublicKey "test")
            "test/golden/WalletError_InvalidPublicKey"

golden_WalletError_UnsignedTxCreationError :: Property
golden_WalletError_UnsignedTxCreationError =
    goldenTestJSON
        UnsignedTxCreationError
            "test/golden/WalletError_UnsignedTxCreationError"

golden_WalletError_SignedTxSubmitError :: Property
golden_WalletError_SignedTxSubmitError =
    goldenTestJSON
        (SignedTxSubmitError "test")
            "test/golden/WalletError_SignedTxSubmitError"

golden_WalletError_TooBigTransaction :: Property
golden_WalletError_TooBigTransaction =
    goldenTestJSON
        TooBigTransaction
            "test/golden/WalletError_TooBigTransaction"

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
        (TxSafeSignerNotFound $ V1 exampleAddress)
            "test/golden/WalletError_TxSafeSignerNotFound"

golden_WalletError_MissingRequiredParams :: Property
golden_WalletError_MissingRequiredParams =
    goldenTestJSON
        (MissingRequiredParams (("test", "test") :| []))
            "test/golden/WalletError_MissingRequiredParams"

golden_WalletError_WalletIsNotReadyToProcessPayments :: Property
golden_WalletError_WalletIsNotReadyToProcessPayments =
    goldenTestJSON
        (WalletIsNotReadyToProcessPayments genExample)
            "test/golden/WalletError_WalletIsNotReadyToProcessPayments"

golden_WalletError_NodeIsStillSyncing :: Property
golden_WalletError_NodeIsStillSyncing =
    goldenTestJSON
        (NodeIsStillSyncing genExample)
            "test/golden/WalletError_NodeIsStillSyncing"

golden_WalletError_CannotCreateAddress :: Property
golden_WalletError_CannotCreateAddress =
    goldenTestJSON
        (CannotCreateAddress "test")
            "test/golden/WalletError_CannotCreateAddress"


-------------------------------------------------------------------------------
-- JSONValidationError
-------------------------------------------------------------------------------
golden_JSONValidationError_JSONValidationFailed :: Property
golden_JSONValidationError_JSONValidationFailed =
    goldenTestJSON
        (JSONValidationFailed "test")
            "test/golden/JSONValidationError_JSONValidationFailed"

-------------------------------------------------------------------------------
-- UnsupportedMimeTypeError
-------------------------------------------------------------------------------
golden_UnsupportedMimeTypeError_UnsupportedMimeTypePresent :: Property
golden_UnsupportedMimeTypeError_UnsupportedMimeTypePresent =
    goldenTestJSON
        (UnsupportedMimeTypePresent "test")
            "test/golden/UnsupportedMimeTypeError_UnsupportedMimeTypePresent"
