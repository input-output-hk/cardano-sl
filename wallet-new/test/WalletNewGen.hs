module WalletNewGen
       ( genWalletError
       ) where

import           Universum

import           Cardano.Wallet.API.V1.Errors (WalletError (..))
import           Cardano.Wallet.API.V1.Types (SyncProgress (..), V1 (..),
                     mkEstimatedCompletionTime, mkSyncPercentage,
                     mkSyncThroughput)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Pos.Core.Common (BlockCount (..))

import           Test.Pos.Core.Gen (genAddress)


genWalletError :: Gen WalletError
genWalletError = Gen.choice
    [ NotEnoughMoney <$> Gen.int (Range.constant 1 1000)
    , OutputIsRedeem . V1 <$> genAddress
    , MigrationFailed <$> Gen.text (Range.constant 1 100) Gen.alphaNum
    , JSONValidationFailed <$> Gen.text (Range.constant 1 100) Gen.alphaNum
    , UnknownError <$> Gen.text (Range.constant 1 100) Gen.alphaNum
    , InvalidAddressFormat <$> Gen.text (Range.constant 1 100) Gen.alphaNum
    , pure WalletNotFound
    , pure WalletAlreadyExists
    , pure AddressNotFound
    , pure TxFailedToStabilize
    , pure TxRedemptionDepleted
    , TxSafeSignerNotFound . V1 <$> genAddress
    , MissingRequiredParams <$> Gen.nonEmpty
          (Range.constant 1 10)
          ((,) <$> Gen.text (Range.constant 1 100) Gen.alphaNum
               <*> Gen.text (Range.constant 1 100) Gen.alphaNum)
    , WalletIsNotReadyToProcessPayments <$> genSyncProgress
    , NodeIsStillSyncing
          <$> (mkSyncPercentage
          <$> Gen.word8 (Range.constant 1 100))
    ]

genSyncProgress :: Gen SyncProgress
genSyncProgress = do
    estCompT <- Gen.word Range.constantBounded
    blockCount <- Gen.word64 Range.constantBounded
    pct <- (Gen.word8 $ Range.constant 0 100)
    pure $ SyncProgress
               (mkEstimatedCompletionTime estCompT)
               (mkSyncThroughput $ BlockCount blockCount)
               (mkSyncPercentage pct)
