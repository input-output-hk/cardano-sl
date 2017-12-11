{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Wallet.API.V1.Types (
  -- * Swagger & REST-related types
    PasswordUpdate (..)
  , AccountUpdate (..)
  , NewAccount (..)
  , Update
  , New
  -- * Domain-specific types
  -- * Wallets
  , Wallet (..)
  , AssuranceLevel (..)
  , NewWallet (..)
  , WalletUpdate (..)
  , WalletId (..)
  , SpendingPassword
  -- * Accounts
  , Account (..)
  , AccountId
  -- * Payments
  , TxId (..)
  , Payment (..)
  , PaymentDistribution (..)
  , Transaction (..)
  , TransactionType (..)
  , TransactionDirection (..)
  , TransactionGroupingPolicy (..)
  , EstimatedFees (..)
  -- * Updates
  , WalletSoftwareUpdate (..)
  -- * Settings
  , NodeSettings (..)
  , SlotDuration
  , mkSlotDuration
  , BlockchainHeight
  , mkBlockchainHeight
  , LocalTimeDifference
  , mkLocalTimeDifference
  , SyncProgress
  , mkSyncProgress
  , NodeInfo (..)
  -- * Core re-exports
  , Core.Address
  ) where

import           Universum

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char as C
import           Data.Text (Text, dropEnd, toLower)
import           Data.Version (Version)
import           GHC.Generics (Generic)
import qualified Serokell.Aeson.Options as Serokell
import           Test.QuickCheck
import           Web.HttpApiData

import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..), UnitOfMeasure (..))
import           Cardano.Wallet.Orphans.Aeson ()

-- V0 logic
import           Pos.Util.BackupPhrase (BackupPhrase)


import           Pos.Aeson.Core ()
import           Pos.Arbitrary.Core ()
import qualified Pos.Core as Core
import qualified Pos.Crypto.Signing as Core

--
-- Domain-specific types, mostly placeholders.
--

-- | A 'SpendingPassword' represent a secret piece of information which can be
-- optionally supplied by the user to encrypt the private keys. As private keys
-- are needed to spend funds and this password secures spending, here the name
-- 'SpendingPassword'.
-- Practically speaking, it's just a type synonym for a PassPhrase, which is a
-- base16-encoded string.
type SpendingPassword = Core.PassPhrase

type WalletName = Text

data AssuranceLevel =
    NormalAssurance
  | StrictAssurance
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary AssuranceLevel where
    arbitrary = elements [minBound .. maxBound]

deriveJSON Serokell.defaultOptions { constructorTagModifier = toString . toLower . dropEnd 9 . fromString
                                   } ''AssuranceLevel

-- | A Wallet ID.
newtype WalletId = WalletId Text deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''WalletId

instance Arbitrary WalletId where
  arbitrary =
      let wid = "J7rQqaLLHBFPrgJXwpktaMB1B1kQBXAyc2uRSfRPzNVGiv6TdxBzkPNBUWysZZZdhFG9gRy3sQFfX5wfpLbi4XTFGFxTg"
          in WalletId . fromString <$> elements [wid]

instance FromHttpApiData WalletId where
    parseQueryParam = Right . WalletId

instance ToHttpApiData WalletId where
    toQueryParam (WalletId wid) = wid

-- | A type modelling the request for a new 'Wallet'.
data NewWallet = NewWallet {
      newwalBackupPhrase     :: !BackupPhrase
    -- ^ The backup phrase to restore the wallet.
    , newwalSpendingPassword :: !(Maybe SpendingPassword)
    -- ^ The spending password to encrypt the private keys.
    , newwalAssuranceLevel   :: !AssuranceLevel
    , newwalName             :: !WalletName
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions  ''NewWallet

instance Arbitrary NewWallet where
  arbitrary = NewWallet <$> arbitrary
                        <*> pure Nothing
                        <*> arbitrary
                        <*> pure "My Wallet"

-- | A type modelling the update of an existing wallet.
data WalletUpdate = WalletUpdate {
      uwalAssuranceLevel :: !AssuranceLevel
    , uwalName           :: !Text
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions  ''WalletUpdate

instance Arbitrary WalletUpdate where
  arbitrary = WalletUpdate <$> arbitrary
                           <*> pure "My Wallet"

-- | A 'Wallet'.
data Wallet = Wallet {
      walId      :: !WalletId
    , walName    :: !WalletName
    , walBalance :: !Core.Coin
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions ''Wallet

instance Arbitrary Wallet where
  arbitrary = Wallet <$> arbitrary
                     <*> pure "My wallet"
                     <*> arbitrary

type AccountId = Word32

-- | A wallet's 'Account'.
data Account = Account
  { accId        :: !AccountId
  , accAddresses :: [Core.Address]
  , accAmount    :: !Core.Coin
  , accName      :: !Text
  -- ^ The Account name.
  , accWalletId  :: WalletId
  -- ^ The 'WalletId' this 'Account' belongs to.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Account

instance Arbitrary Account where
  arbitrary = Account <$> arbitrary
                      <*> listOf1 arbitrary
                      -- ^ FIXME(adinapoli): It's crucial we revert this
                      -- instance to be simply 'arbitrary' as an 'Account'
                      -- can easily have 0 or more addresses. However, we
                      -- need to implement CSL-2034 first, otherwise the
                      -- generated Swagger specs won't be valid, because they
                      -- will contain a duplicate entry in the 'items' directive
                      -- for a 'WalletResponse [Account]', which I'm not sure if
                      -- it's a bug in @servant-swagger@ or in our code.
                      <*> arbitrary
                      <*> pure "My account"
                      <*> arbitrary

data AccountUpdate = AccountUpdate {
    uaccName      :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''AccountUpdate

instance Arbitrary AccountUpdate where
  arbitrary = AccountUpdate . fromString <$> pure "myAccount"

data NewAccount = NewAccount
  { naccSpendingPassword :: !(Maybe SpendingPassword)
  , naccName             :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''NewAccount

instance Arbitrary NewAccount where
  arbitrary = NewAccount <$> arbitrary
                         <*> arbitrary

-- | A type incapsulating a password update request.
data PasswordUpdate = PasswordUpdate {
    pwdOld :: !SpendingPassword
    -- ^ The old 'SpendingPassword'.
  , pwdNew :: !SpendingPassword
    -- ^ The new 'SpendingPassword'.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PasswordUpdate

instance Arbitrary PasswordUpdate where
  arbitrary = PasswordUpdate <$> arbitrary
                             <*> arbitrary

-- | 'EstimatedFees' represents the fees which would be generated
-- for a 'Payment' in case the latter would actually be performed.
data EstimatedFees = EstimatedFees {
    feeEstimatedAmount :: !Core.Coin
    -- ^ The estimated fees, as coins.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''EstimatedFees

instance Arbitrary EstimatedFees where
  arbitrary = EstimatedFees <$> arbitrary

-- | Maps an 'Address' to some 'Coin's, and it's
-- typically used to specify where to send money during a 'Payment'.
data PaymentDistribution = PaymentDistribution {
      pdAddress :: Core.Address
    , pdAmount  :: Core.Coin
    } deriving (Show, Eq)

deriveJSON Serokell.defaultOptions ''PaymentDistribution

instance Arbitrary PaymentDistribution where
  arbitrary = PaymentDistribution <$> arbitrary
                                  <*> arbitrary

-- | A policy to be passed to each new payment request to
-- determine how a 'Transaction' is assembled.
data TransactionGroupingPolicy =
    OptimiseForSizePolicy
  -- ^ Tries to minimise the size of the created transaction
  -- by choosing only the biggest value available up until
  -- the stake sum is greater or equal the payment amount.
  | OptimiseForSecurityPolicy
  -- ^ Tries to minimise the number of addresses left with
  -- unspent funds after the transaction has been created.
  deriving (Show, Ord, Eq, Enum, Bounded)

instance Arbitrary TransactionGroupingPolicy where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Policy@ suffix.
deriveJSON defaultOptions { constructorTagModifier = reverse . drop 6 . reverse } ''TransactionGroupingPolicy

-- | A 'Payment' from one source account to one or more 'PaymentDistribution'(s).
data Payment = Payment
  { pmtSourceWallet   :: !WalletId
    -- ^ The source Wallet.
  , pmtSourceAccount  :: !AccountId
    -- ^ The source Account.
  , pmtDestinations   :: !(NonEmpty PaymentDistribution)
    -- ^ The destinations for this payment.
  , pmtGroupingPolicy :: !(Maybe TransactionGroupingPolicy)
    -- ^ Which strategy use in grouping the input transactions.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Payment

instance Arbitrary Payment where
  arbitrary = Payment <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

----------------------------------------------------------------------------
-- TxId
----------------------------------------------------------------------------

-- | TxId
newtype TxId = TxId Text
    deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''TxId

instance Arbitrary TxId where
  arbitrary = TxId . fromString <$> elements
      [ "1f434ae9e903ea86f420cd18160d2a6c4d5efa29a1004dfb0466f7a2ec643a6d"
      , "b53fadd178f752271cfd079aeaf2b791870ede4ed1456d889e43273a8cef87fb"
      , "a792424d01bbba9fcdf129f40cdde3808baa7c9c38f898e40e7545358a093ca6"
      ]

instance FromHttpApiData TxId where
    parseQueryParam = Right . TxId

instance ToHttpApiData TxId where
    toQueryParam (TxId txId) = txId

----------------------------------------------------------------------------
  -- Transaction types
----------------------------------------------------------------------------

-- | The 'Transaction' type.
data TransactionType =
    LocalTransaction
  -- ^ This transaction is local, which means all the inputs
  -- and all the outputs belongs to the wallet from which the
  -- transaction was originated.
  | ForeignTransaction
  -- ^ This transaction is not local to this wallet.
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary TransactionType where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Transaction@ suffix.
deriveJSON defaultOptions { constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionType

-- | The 'Transaction' @direction@
data TransactionDirection =
    IncomingTransaction
  -- ^ This represents an incoming transactions.
  | OutgoingTransaction
  -- ^ This qualifies external transactions.
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary TransactionDirection where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Transaction@ suffix.
deriveJSON defaultOptions { constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionDirection

-- | A 'Wallet''s 'Transaction'.
data Transaction = Transaction
  { txId            :: !TxId
    -- ^ The Tx Id.
  , txConfirmations :: !Word
    -- ^ The number of confirmations.
  , txAmount        :: !Core.Coin
    -- ^ The 'Coin' moved as part of this transaction.
  , txInputs        :: !(NonEmpty PaymentDistribution)
    -- ^ The input money distribution.
  , txOutputs       :: !(NonEmpty PaymentDistribution)
    -- ^ The output money distribution.
  , txType          :: TransactionType
    -- ^ The type for this transaction (e.g local, foreign, etc).
  , txDirection     :: TransactionDirection
    -- ^ The direction for this transaction (e.g incoming, outgoing).
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Transaction

instance Arbitrary Transaction where
  arbitrary = Transaction <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

-- | A type representing an upcoming wallet update.
data WalletSoftwareUpdate = WalletSoftwareUpdate
  { updSoftwareVersion   :: !Text
  , updBlockchainVersion :: !Text
  , updScriptVersion     :: !Int
  -- Other types omitted for now.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''WalletSoftwareUpdate

instance Arbitrary WalletSoftwareUpdate where
  arbitrary = WalletSoftwareUpdate <$> fmap fromString arbitrary
                                   <*> fmap fromString arbitrary
                                   <*> fmap getPositive arbitrary

-- | How many milliseconds a slot lasts for.
newtype SlotDuration = SlotDuration (MeasuredIn 'Milliseconds Word)
                     deriving (Show, Eq)

mkSlotDuration :: Word -> SlotDuration
mkSlotDuration = SlotDuration . MeasuredIn

instance Arbitrary SlotDuration where
    arbitrary = mkSlotDuration <$> choose (0, 100)

instance ToJSON SlotDuration where
    toJSON (SlotDuration (MeasuredIn w)) =
            object [ "quantity" .= toJSON w
                   , "unit"     .= String "milliseconds"
                   ]

instance FromJSON SlotDuration where
    parseJSON = withObject "SlotDuration" $ \sl -> mkSlotDuration <$> sl .: "quantity"

-- | The @static@ settings for this wallet node. In particular, we could group
-- here protocol-related settings like the slot duration, the transaction max size,
-- the current software version running on the node, etc.
data NodeSettings = NodeSettings {
     setSlotDuration   :: !SlotDuration
   , setSoftwareInfo   :: !Core.SoftwareVersion
   , setProjectVersion :: !Version
   , setGitRevision    :: !Text
   } deriving (Show, Eq)

-- The following instances are derived manually due to the fact that changing the
-- way `SoftwareVersion` is represented would break compatibility with V0, so the
-- solution is either write this manual instance by hand or create a `newtype` wrapper.
--deriveJSON Serokell.defaultOptions ''NodeSettings
instance ToJSON NodeSettings where
    toJSON NodeSettings{..} =
        let override Core.SoftwareVersion{..} =
                object [ "applicationName" .= toJSON (Core.getApplicationName svAppName)
                       , "version" .=  toJSON svNumber
                       ]
        in object [ "slotDuration" .= toJSON setSlotDuration
                  , "softwareInfo" .= override setSoftwareInfo
                  , "projectVersion" .= toJSON setProjectVersion
                  , "gitRevision" .= toJSON setGitRevision
                  ]

instance FromJSON NodeSettings where
    parseJSON = withObject "NodeSettings" $ \ns -> do
        si <- ns .: "softwareInfo"
        let softwareInfo = withObject "SoftwareVersion" $ \sw ->
                Core.SoftwareVersion <$> sw .: "applicationName"
                                     <*> sw .: "version"
        NodeSettings <$> ns .: "slotDuration"
                     <*> softwareInfo si
                     <*> ns .: "projectVersion"
                     <*> ns .: "gitRevision"

instance Arbitrary NodeSettings where
    arbitrary = NodeSettings <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> pure "0e1c9322a"

-- | The different between the local time and the remote NTP server.
newtype LocalTimeDifference = LocalTimeDifference (MeasuredIn 'Microseconds Word)
                            deriving (Show, Eq)

mkLocalTimeDifference :: Word -> LocalTimeDifference
mkLocalTimeDifference = LocalTimeDifference . MeasuredIn

instance Arbitrary LocalTimeDifference where
    arbitrary = mkLocalTimeDifference <$> choose (minBound, maxBound)

instance ToJSON LocalTimeDifference where
    toJSON (LocalTimeDifference (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "microseconds"
               ]

instance FromJSON LocalTimeDifference where
    parseJSON = withObject "LocalTimeDifference" $ \sl -> mkLocalTimeDifference <$> sl .: "quantity"

-- | The sync progress with the blockchain.
newtype SyncProgress = SyncProgress (MeasuredIn 'Percentage100 Word8)
                     deriving (Show, Eq)

mkSyncProgress :: Word8 -> SyncProgress
mkSyncProgress = SyncProgress . MeasuredIn

instance Arbitrary SyncProgress where
    arbitrary = mkSyncProgress <$> choose (0, 100)

instance ToJSON SyncProgress where
    toJSON (SyncProgress (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "percent"
               ]

instance FromJSON SyncProgress where
    parseJSON = withObject "SyncProgress" $ \sl -> mkSyncProgress <$> sl .: "quantity"

-- | The absolute or relative height of the blockchain, measured in number
-- of blocks.
newtype BlockchainHeight = BlockchainHeight (MeasuredIn 'Blocks Core.BlockCount)
                         deriving (Show, Eq)

mkBlockchainHeight :: Core.BlockCount -> BlockchainHeight
mkBlockchainHeight = BlockchainHeight . MeasuredIn

instance Arbitrary BlockchainHeight where
    arbitrary = mkBlockchainHeight . Core.BlockCount <$> choose (minBound, maxBound)

instance ToJSON BlockchainHeight where
    toJSON (BlockchainHeight (MeasuredIn w)) = object [ "quantity" .= toJSON (Core.getBlockCount w)
                                                      , "unit"     .= String "blocks"
                                                      ]

instance FromJSON BlockchainHeight where
    parseJSON = withObject "BlockchainHeight" $ \sl ->
        mkBlockchainHeight . Core.BlockCount <$> sl .: "quantity"

-- | The @dynamic@ information for this node.
data NodeInfo = NodeInfo {
     nfoSyncProgress          :: !SyncProgress
   , nfoBlockchainHeight      :: !(Maybe BlockchainHeight)
   -- ^ The current blockchain "height", in blocks, if we know it.
   , nfoLocalBlockchainHeight :: !BlockchainHeight
   -- ^ The local blockchain "height", in blocks.
   , nfoLocalTimeDifference   :: !LocalTimeDifference
   } deriving (Show, Eq)

deriveJSON Serokell.defaultOptions ''NodeInfo

instance Arbitrary NodeInfo where
    arbitrary = NodeInfo <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

--
-- POST/PUT requests isomorphisms
--

type family Update (original :: *) :: * where
  Update Wallet  = WalletUpdate
  Update Account = AccountUpdate

type family New (original :: *) :: * where
  New Wallet  = NewWallet
  New Account = NewAccount
