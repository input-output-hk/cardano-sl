module Cardano.Wallet.API.V1.Swagger.Example where

import           Universum

import           Test.QuickCheck (Arbitrary (..), Gen, listOf1, oneof)

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Orphans.Arbitrary ()
import           Data.Default (Default (def))
import           Node (NodeId (..))
import           Pos.Arbitrary.Wallet.Web.ClientTypes ()
import           Pos.Client.Txp.Util (InputSelectionPolicy (..))
import           Pos.Util.BackupPhrase (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes (CUpdateInfo)
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot (..))

import qualified Data.Map.Strict as Map
import qualified Pos.Core.Common as Core
import qualified Pos.Crypto.Signing as Core


class Arbitrary a => Example a where
    example :: Gen a
    example = arbitrary

instance Example ()
instance Example a => Example (NonEmpty a)

-- NOTE: we don't want to see empty list examples in our swagger doc :)
instance Example a => Example [a] where
    example = listOf1 example

-- NOTE: we don't want to see "null" examples in our swagger doc :)
instance Example a => Example (Maybe a) where
    example = Just <$> example

-- NOTE: we don't want to see empty maps in our swagger doc :)
instance (Ord k, Example k, Example v) => Example (Map k v) where
    example = Map.fromList <$> listOf1 ((,) <$> example <*> example)

instance Example (V1 Core.PassPhrase)
instance Example (V1 Core.Coin)

instance Example a => Example (WalletResponse a) where
    example = WalletResponse <$> example
                             <*> pure SuccessStatus
                             <*> example

-- | We have a specific 'Example' instance for @'V1' 'Address'@ because we want
-- to control the length of the examples. It is possible for the encoded length
-- to become huge, up to 1000+ bytes, if the 'UnsafeMultiKeyDistr' constructor
-- is used. We do not use this constructor, which keeps the address between
-- ~80-150 bytes long.
instance Example (V1 Address) where
    example = fmap V1 . Core.makeAddress
        <$> arbitrary
        <*> arbitraryAttributes
      where
        arbitraryAttributes =
            Core.AddrAttributes
                <$> arbitrary
                <*> oneof
                    [ pure Core.BootstrapEraDistr
                    , Core.SingleKeyDistr <$> arbitrary
                    ]
                <*> arbitrary

instance Example BackupPhrase where
    example = pure def

instance Example Address
instance Example Metadata
instance Example AccountIndex
instance Example WalletId
instance Example (V1 BackupPhrase)
instance Example AssuranceLevel
instance Example SyncPercentage
instance Example BlockchainHeight
instance Example LocalTimeDifference
instance Example PaymentDistribution
instance Example AccountUpdate
instance Example Wallet
instance Example WalletUpdate
instance Example WalletOperation
instance Example PasswordUpdate
instance Example EstimatedFees
instance Example Transaction
instance Example WalletSoftwareUpdate
instance Example NodeSettings
instance Example SlotDuration
instance Example WalletAddress
instance Example NewAccount
instance Example TimeInfo
instance Example AddressValidity
instance Example NewAddress
instance Example CUpdateInfo
instance Example SubscriptionStatus
instance Example NodeId

instance Example InputSelectionPolicy where
    example = pure OptimizeForHighThroughput

instance Example (V1 InputSelectionPolicy) where
    example = pure (V1 OptimizeForHighThroughput)

instance Example Account where
    example = Account <$> example
                      <*> example -- NOTE: this will produce non empty list
                      <*> example
                      <*> pure "My account"
                      <*> example

instance Example NewWallet where
    example = NewWallet <$> example
                        <*> example -- Note: will produce `Just a`
                        <*> example
                        <*> pure "My Wallet"
                        <*> example

instance Example NodeInfo where
    example = NodeInfo <$> example
                       <*> example  -- NOTE: will produce `Just a`
                       <*> example
                       <*> example
                       <*> example

instance Example PaymentSource where
    example = PaymentSource <$> example
                            <*> example

instance Example Payment where
    example = Payment <$> example
                      <*> example
                      <*> example -- TODO: will produce `Just groupingPolicy`
                      <*> example

instance Example WalletStateSnapshot



-- IMPORTANT: if executing `grep "[]\|null" wallet-new/spec/swagger.json` returns any element - then we have to add Example instances for those objects because we don't want to see [] or null examples in our docs.
--
-- TODO: We should probably add this as a part of our swagger CI script and fail swagger if we find some of them - with instruction to the developer above what is said above.
--
-- Most of it comes to removing Nothing from `Arbitrary (Maybe a)` instance and removing empty list from `Arbitrary [a]` instance. It could be done automatically with some quickcheck hacks but I think it would be an overkill.
