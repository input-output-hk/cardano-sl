-- | HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet (
    -- * Supporting types
    WalletName(..)
  , AccountName(..)
  , AccountIx(..)
  , AddressIx(..)
  , AssuranceLevel(..)
  , HasSpendingPassword(..)
    -- * HD proper
  , HdRootId(..)
  , HdAccountId(..)
  , HdAddressId(..)
  , HdRoot(..)
  , HdAccount(..)
  , HdAddress(..)
    -- ** Lenses
  , hdRootName
  , hdRootHasPassword
  , hdRootAssurance
  , hdRootCreatedAt
  , hdRootAccounts
  , hdAccountName
  , hdAccountAddresses
  , hdAccountCheckpoints
  , hdAddressAddress
  , hdAddressIsUsed
  , hdAddressIsChange
    -- * Derived information
  , hdRootBalance
  , hdAccountBalance
    -- * Unknown identifiers
  , UnknownHdRoot(..)
  , UnknownHdAccount(..)
  , UnknownHdAddress(..)
  , embedUnknownHdRoot
  , embedUnknownHdAccount
    -- * Zoom to parts of the HD wallet
  , HdRoots
  , zoomHdRootId
  , zoomHdAccountId
  , zoomHdAddressId
  ) where

import           Universum

import           Control.Lens (at, toListOf)
import           Control.Lens.TH (makeLenses)
import           Data.SafeCopy (base, deriveSafeCopy)

import qualified Pos.Core as Core
import qualified Pos.Crypto as Core

import           Cardano.Wallet.Kernel.DB.AcidStateUtil
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

-- | Wallet name
newtype WalletName = WalletName Text

-- | Account name
newtype AccountName = AccountName Text

-- | Account index
newtype AccountIx = AccountIx Word32
  deriving (Eq, Ord)

-- | Address index
newtype AddressIx = AddressIx Word32
  deriving (Eq, Ord)

-- | Wallet assurance level
--
-- TODO: document what these levels mean (in particular, how it does translate
-- to the depth required before a transaction is marked as Persisted?)
data AssuranceLevel =
    AssuranceLevelNormal
  | AssuranceLevelStrict

-- | Does this wallet have a spending password
data HasSpendingPassword =
    -- | No spending password set
    NoSpendingPassword

    -- | If there is a spending password, we record when it was last updated.
  | HasSpendingPassword (InDb Core.Timestamp)

deriveSafeCopy 1 'base ''WalletName
deriveSafeCopy 1 'base ''AccountName
deriveSafeCopy 1 'base ''AccountIx
deriveSafeCopy 1 'base ''AddressIx
deriveSafeCopy 1 'base ''AssuranceLevel
deriveSafeCopy 1 'base ''HasSpendingPassword

{-------------------------------------------------------------------------------
  HD wallets
-------------------------------------------------------------------------------}

-- | HD wallet root ID
data HdRootId = HdRootId (InDb (Core.AddressHash Core.PublicKey))
  deriving (Eq, Ord)

-- | HD wallet account ID
data HdAccountId = HdAccountId HdRootId AccountIx
  deriving (Eq, Ord)

-- | HD wallet address ID
data HdAddressId = HdAddressId HdAccountId AddressIx
  deriving (Eq, Ord)

-- | Root of a HD wallet
--
-- The wallet has sequentially assigned account indices and randomly assigned
-- address indices.
--
-- NOTE: We do not store the encrypted key of the wallet.
--
-- TODO: synchronization state
data HdRoot = HdRoot {
      -- | Wallet name
      _hdRootName        :: WalletName

      -- | Does this wallet have a spending password?
      --
      -- NOTE: We do not store the spending password itself, but merely record
      -- whether there is one. Updates to the spending password affect only the
      -- external key storage, not the wallet DB proper.
    , _hdRootHasPassword :: HasSpendingPassword

      -- | Assurance level
    , _hdRootAssurance   :: AssuranceLevel

      -- | When was this wallet created?
    , _hdRootCreatedAt   :: InDb Core.Timestamp

      -- | Known derived accounts
    , _hdRootAccounts    :: Map AccountIx HdAccount
    }

-- | Account in a HD wallet
--
-- Key derivation is cheap
data HdAccount = HdAccount {
      -- | Account name
      _hdAccountName        :: AccountName

      -- | Known derived addresses
    , _hdAccountAddresses   :: Map AddressIx HdAddress

      -- | State of the " wallet " as stipulated by the wallet specification
    , _hdAccountCheckpoints :: NonEmpty Checkpoint
    }

-- | Address in an account of a HD wallet
data HdAddress = HdAddress {
      -- | The actual address
      _hdAddressAddress  :: InDb Core.Address

      -- | Has this address been involved in a transaction?
      --
      -- TODO: How is this determined? What is the definition? How is it set?
    , _hdAddressIsUsed   :: Bool

      -- | Was this address used as a change address?
      --
      -- TODO: How is this derived when we do wallet recovery?
      -- TODO: Do we need this at all?
    , _hdAddressIsChange :: Bool
    }

makeLenses ''HdRoot
makeLenses ''HdAccount
makeLenses ''HdAddress

deriveSafeCopy 1 'base ''HdRootId
deriveSafeCopy 1 'base ''HdAccountId
deriveSafeCopy 1 'base ''HdAddressId

deriveSafeCopy 1 'base ''HdRoot
deriveSafeCopy 1 'base ''HdAccount
deriveSafeCopy 1 'base ''HdAddress

{-------------------------------------------------------------------------------
  Derived information
-------------------------------------------------------------------------------}

-- | Total balance of a wallet
--
-- This returns an integer because we may otherwise run into overflow.
hdRootBalance :: HdRoot -> Integer
hdRootBalance = Core.sumCoins
              . toListOf ( hdRootAccounts
                         . traverse
                         . hdAccountCheckpoints
                         . currentUtxoBalance
                         )

-- | Current balance on an account
hdAccountBalance :: HdAccount -> Core.Coin
hdAccountBalance = view (hdAccountCheckpoints . currentUtxoBalance)

{-------------------------------------------------------------------------------
  Unknown identifiers
-------------------------------------------------------------------------------}

-- | Unknown root
data UnknownHdRoot =
    -- | Unknown root ID
    UnknownHdRoot HdRootId

-- | Unknown account
data UnknownHdAccount =
    -- | Unknown root ID
    UnknownHdAccountRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAccount HdAccountId

-- | Unknown address
data UnknownHdAddress =
    -- | Unknown root ID
    UnknownHdAddressRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAddressAccount HdAccountId

    -- | Unknown address (implies the account is known)
  | UnknownHdAddress HdAddressId

embedUnknownHdRoot :: UnknownHdRoot -> UnknownHdAccount
embedUnknownHdRoot = go
  where
    go (UnknownHdRoot rootId) = UnknownHdAccountRoot rootId

embedUnknownHdAccount :: UnknownHdAccount -> UnknownHdAddress
embedUnknownHdAccount = go
  where
    go (UnknownHdAccountRoot rootId) = UnknownHdAddressRoot rootId
    go (UnknownHdAccount accountId)  = UnknownHdAddressAccount accountId

deriveSafeCopy 1 'base ''UnknownHdRoot
deriveSafeCopy 1 'base ''UnknownHdAddress
deriveSafeCopy 1 'base ''UnknownHdAccount

{-------------------------------------------------------------------------------
  Zoom to parts of a HD wallet
-------------------------------------------------------------------------------}

type HdRoots = Map HdRootId HdRoot

zoomHdRootId :: (UnknownHdRoot -> e)
             -> HdRootId
             -> Update' HdRoot e a -> Update' HdRoots e a
zoomHdRootId embedErr rootId =
      zoomTry (embedErr $ UnknownHdRoot rootId) (at rootId)

zoomHdAccountId :: (UnknownHdAccount -> e)
                -> HdAccountId
                -> Update' HdAccount e a -> Update' HdRoots e a
zoomHdAccountId embedErr accId@(HdAccountId rootId accIx) =
      zoomHdRootId (embedErr . embedUnknownHdRoot) rootId
    . zoom hdRootAccounts
    . zoomTry (embedErr $ UnknownHdAccount accId) (at accIx)

zoomHdAddressId :: (UnknownHdAddress -> e)
                -> HdAddressId
                -> Update' HdAddress e a -> Update' HdRoots e a
zoomHdAddressId embedErr addrId@(HdAddressId accId addrIx) =
      zoomHdAccountId (embedErr . embedUnknownHdAccount) accId
    . zoom hdAccountAddresses
    . zoomTry (embedErr $ UnknownHdAddress addrId) (at addrIx)
