-- | HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet (
    -- * Supporting types
    WalletName(..)
  , AccountName(..)
  , HdAccountIx(..)
  , HdAddressIx(..)
  , AssuranceLevel(..)
  , HasSpendingPassword(..)
    -- * HD wallet types proper
  , HdWallets(..)
  , HdRootId(..)
  , HdAccountId(..)
  , HdAddressId(..)
  , HdRoot(..)
  , HdAccount(..)
  , HdAddress(..)
    -- ** Lenses
  , hdWalletsRoots
  , hdWalletsAccounts
  , hdWalletsAddresses
  , hdAccountIdParent
  , hdAccountIdIx
  , hdAddressIdParent
  , hdAddressIdIx
  , hdRootId
  , hdRootName
  , hdRootHasPassword
  , hdRootAssurance
  , hdRootCreatedAt
  , hdAccountId
  , hdAccountName
  , hdAccountCheckpoints
  , hdAddressId
  , hdAddressAddress
  , hdAddressIsUsed
  , hdAddressIsChange
    -- ** Composite lenses
  , hdAccountRootId
  , hdAddressRootId
  , hdAddressAccountId
    -- * Unknown identifiers
  , UnknownHdRoot(..)
  , UnknownHdAccount(..)
  , UnknownHdAddress(..)
  , embedUnknownHdRoot
  , embedUnknownHdAccount
    -- * Zoom to parts of the HD wallet
  , zoomHdRootId
  , zoomHdAccountId
  , zoomHdAddressId
  ) where

import           Universum

import           Control.Lens (at)
import           Control.Lens.TH (makeLenses)
import qualified Data.IxSet.Typed as IxSet
import           Data.SafeCopy (base, deriveSafeCopy)

import qualified Pos.Core as Core
import qualified Pos.Crypto as Core

import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

-- | Wallet name
newtype WalletName = WalletName Text

-- | Account name
newtype AccountName = AccountName Text

-- | Account index
newtype HdAccountIx = HdAccountIx Word32
  deriving (Eq, Ord)

-- | Address index
newtype HdAddressIx = HdAddressIx Word32
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
deriveSafeCopy 1 'base ''HdAccountIx
deriveSafeCopy 1 'base ''HdAddressIx
deriveSafeCopy 1 'base ''AssuranceLevel
deriveSafeCopy 1 'base ''HasSpendingPassword

{-------------------------------------------------------------------------------
  HD wallets
-------------------------------------------------------------------------------}

-- | HD wallet root ID
data HdRootId = HdRootId (InDb (Core.AddressHash Core.PublicKey))
  deriving (Eq, Ord)

-- | HD wallet account ID
data HdAccountId = HdAccountId {
      _hdAccountIdParent :: HdRootId
    , _hdAccountIdIx     :: HdAccountIx
    }
  deriving (Eq, Ord)

-- | HD wallet address ID
data HdAddressId = HdAddressId {
      _hdAddressIdParent :: HdAccountId
    , _hdAddressIdIx     :: HdAddressIx
    }
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
      -- | Wallet ID
      _hdRootId          :: HdRootId

      -- | Wallet name
    , _hdRootName        :: WalletName

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
    }

-- | Account in a HD wallet
--
-- Key derivation is cheap
data HdAccount = HdAccount {
      -- | Account index
      _hdAccountId          :: HdAccountId

      -- | Account name
    , _hdAccountName        :: AccountName

      -- | State of the " wallet " as stipulated by the wallet specification
    , _hdAccountCheckpoints :: NonEmpty Checkpoint
    }

-- | Address in an account of a HD wallet
data HdAddress = HdAddress {
      -- | Address ID
      _hdAddressId       :: HdAddressId

      -- | The actual address
    , _hdAddressAddress  :: InDb Core.Address

      -- | Has this address been involved in a transaction?
      --
      -- TODO: How is this determined? What is the definition? How is it set?
      -- TODO: This will likely move to the 'BlockMeta' instead.
    , _hdAddressIsUsed   :: Bool

      -- | Was this address used as a change address?
      --
      -- TODO: How is this derived when we do wallet recovery?
      -- TODO: Do we need this at all?
    , _hdAddressIsChange :: Bool
    }

makeLenses ''HdAccountId
makeLenses ''HdAddressId

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
  Derived lenses
-------------------------------------------------------------------------------}

hdAccountRootId :: Lens' HdAccount HdRootId
hdAccountRootId = hdAccountId . hdAccountIdParent

hdAddressAccountId :: Lens' HdAddress HdAccountId
hdAddressAccountId = hdAddressId . hdAddressIdParent

hdAddressRootId :: Lens' HdAddress HdRootId
hdAddressRootId = hdAddressAccountId . hdAccountIdParent

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
  IxSet instantiations
-------------------------------------------------------------------------------}

instance HasPrimKey HdRoot where
    type PrimKey HdRoot = HdRootId
    primKey = _hdRootId

instance HasPrimKey HdAccount where
    type PrimKey HdAccount = HdAccountId
    primKey = _hdAccountId

instance HasPrimKey HdAddress where
    type PrimKey HdAddress = HdAddressId
    primKey = _hdAddressId

type HdRootIxs    = '[]
type HdAccountIxs = '[HdRootId]
type HdAddressIxs = '[HdRootId, HdAccountId]

type instance IndicesOf HdRoot    = HdRootIxs
type instance IndicesOf HdAccount = HdAccountIxs
type instance IndicesOf HdAddress = HdAddressIxs

instance IxSet.Indexable (HdRootId ': HdRootIxs)
                         (OrdByPrimKey HdRoot) where
    indices = ixList

instance IxSet.Indexable (HdAccountId ': HdAccountIxs)
                         (OrdByPrimKey HdAccount) where
    indices = ixList
                (ixFun ((:[]) . view hdAccountRootId))

instance IxSet.Indexable (HdAddressId ': HdAddressIxs)
                         (OrdByPrimKey HdAddress) where
    indices = ixList
                (ixFun ((:[]) . view hdAddressRootId))
                (ixFun ((:[]) . view hdAddressAccountId))

{-------------------------------------------------------------------------------
  Zoom to parts of a HD wallet
-------------------------------------------------------------------------------}

-- | All wallets, accounts and addresses in the HD wallets
--
-- We use a flat "relational" structure rather than nested maps so that we can
-- go from address to wallet just as easily as the other way around.
data HdWallets = HdWallets {
    _hdWalletsRoots     :: IxSet HdRoot
  , _hdWalletsAccounts  :: IxSet HdAccount
  , _hdWalletsAddresses :: IxSet HdAddress
  }

deriveSafeCopy 1 'base ''HdWallets
makeLenses ''HdWallets

zoomHdRootId :: forall e a.
                (UnknownHdRoot -> e)
             -> HdRootId
             -> Update' HdRoot e a -> Update' HdWallets e a
zoomHdRootId embedErr rootId =
    zoomDef err (hdWalletsRoots . at rootId)
  where
    err :: Update' HdWallets e a
    err = throwError $ embedErr (UnknownHdRoot rootId)

zoomHdAccountId :: forall e a.
                   (UnknownHdAccount -> e)
                -> HdAccountId
                -> Update' HdAccount e a -> Update' HdWallets e a
zoomHdAccountId embedErr accId =
    zoomDef err (hdWalletsAccounts . at accId)
  where
    err :: Update' HdWallets e a
    err = zoomHdRootId embedErr' (accId ^. hdAccountIdParent) $
            throwError $ embedErr (UnknownHdAccount accId)

    embedErr' :: UnknownHdRoot -> e
    embedErr' = embedErr . embedUnknownHdRoot

zoomHdAddressId :: forall e a.
                   (UnknownHdAddress -> e)
                -> HdAddressId
                -> Update' HdAddress e a -> Update' HdWallets e a
zoomHdAddressId embedErr addrId =
    zoomDef err (hdWalletsAddresses . at addrId)
  where
    err :: Update' HdWallets e a
    err = zoomHdAccountId embedErr' (addrId ^. hdAddressIdParent) $
            throwError $ embedErr (UnknownHdAddress addrId)

    embedErr' :: UnknownHdAccount -> e
    embedErr' = embedErr . embedUnknownHdAccount
