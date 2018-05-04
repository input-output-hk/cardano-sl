-- | READ queries on the HD wallet
--
-- NOTE: These are pure functions, which are intended to work on a snapshot
-- of the database. They are intended to support the V1 wallet API.
--
-- TODO: We need to think about which layer will have the responsibility for
-- filtering and sorting. If we want the 'IxSet' stuff to be local to the
-- "Kernel.DB" namespace (which would be a good thing), then filtering and
-- sorting (and maybe even pagination) will need to happen here.
module Cardano.Wallet.Kernel.DB.HdWallet.Read (
    -- | * Infrastructure
    HdQuery
  , HdQueryErr
    -- | * Derived balance
  , hdRootBalance
  , hdAccountBalance
    -- | Accumulate all accounts/addresses
  , readAllHdRoots
  , readAllHdAccounts
  , readAllHdAddresses
    -- | All wallets/accounts/addresses
  , readAccountsByRootId
  , readAddressesByRootId
  , readAddressesByAccountId
    -- | Single wallets/accounts/addresses
  , readHdRoot
  , readHdAccount
  , readHdAddress
  ) where

import           Universum

import           Control.Lens (at)

import           Pos.Core (Coin, sumCoins)

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

-- | Query on a HD wallet
type HdQuery a = HdWallets -> a

-- | Like 'HdQuery', but with the possibility of errors
type HdQueryErr e a = HdQuery (Either e a)

-- | Like '(>>=)' for queries
using :: HdQueryErr e a -> (a -> HdQueryErr e b) -> HdQueryErr e b
using f g wallets =
    case f wallets of
      Left  e -> Left e
      Right a -> g a wallets

-- | Variation on 'using' where the second query cannot throw errors
using' :: HdQueryErr e a -> (a -> HdQuery b) -> HdQueryErr e b
using' f g = using f ((Right .) . g)

-- | Variation on 'using'' where the result of the first query is ignored
--
-- Useful when the first query is merely a sanity check.
check :: HdQueryErr e a -> HdQuery b -> HdQueryErr e b
check f g = using' f (const g)

{-------------------------------------------------------------------------------
  Computed balances information
-------------------------------------------------------------------------------}

hdRootBalance :: HdRootId -> HdQuery Integer
hdRootBalance rootId = sumCoins
                     . map hdAccountBalance
                     . toList
                     . IxSet.getEQ rootId
                     . view hdWalletsAccounts

-- | Current balance of an account
hdAccountBalance :: HdAccount -> Coin
hdAccountBalance = view (hdAccountCheckpoints . currentUtxoBalance)

{-------------------------------------------------------------------------------
  Accumulate across wallets/accounts
-------------------------------------------------------------------------------}

-- | Meta-information of /all wallets
readAllHdRoots :: HdQuery (IxSet HdRoot)
readAllHdRoots = view hdWalletsRoots

-- | Meta-information of /all/ accounts
readAllHdAccounts :: HdQuery (IxSet HdAccount)
readAllHdAccounts = view hdWalletsAccounts

-- | Meta-information and total balance of /all/ addresses
readAllHdAddresses :: HdQuery (IxSet HdAddress)
readAllHdAddresses = view hdWalletsAddresses

{-------------------------------------------------------------------------------
  Information about all wallets/accounts/addresses
-------------------------------------------------------------------------------}

-- | All accounts in the given wallet
readAccountsByRootId :: HdRootId  -> HdQueryErr UnknownHdRoot (IxSet HdAccount)
readAccountsByRootId rootId =
      check (readHdRoot rootId)
    $ IxSet.getEQ rootId . readAllHdAccounts

-- | All addresses in the given wallet
readAddressesByRootId :: HdRootId -> HdQueryErr UnknownHdRoot (IxSet HdAddress)
readAddressesByRootId rootId =
      check (readHdRoot rootId)
    $ IxSet.getEQ rootId . readAllHdAddresses

-- | All addresses in the given account
readAddressesByAccountId :: HdAccountId -> HdQueryErr UnknownHdAccount (IxSet HdAddress)
readAddressesByAccountId accId =
      check (readHdAccount accId)
    $ IxSet.getEQ accId . readAllHdAddresses

{-------------------------------------------------------------------------------
  Information about a single wallet/address/account
-------------------------------------------------------------------------------}

-- | Look up the specified wallet
readHdRoot :: HdRootId -> HdQueryErr UnknownHdRoot HdRoot
readHdRoot rootId = aux . view (at rootId) . readAllHdRoots
  where
    aux :: Maybe a -> Either UnknownHdRoot a
    aux = maybe (Left (UnknownHdRoot rootId)) Right

-- | Look up the specified account
readHdAccount :: HdAccountId -> HdQueryErr UnknownHdAccount HdAccount
readHdAccount accId = aux . view (at accId) . readAllHdAccounts
  where
    aux :: Maybe a -> Either UnknownHdAccount a
    aux = maybe (Left (UnknownHdAccount accId)) Right

-- | Look up the specified address
readHdAddress :: HdAddressId -> HdQueryErr UnknownHdAddress HdAddress
readHdAddress addrId = aux . view (at addrId) . readAllHdAddresses
  where
    aux :: Maybe a -> Either UnknownHdAddress a
    aux = maybe (Left (UnknownHdAddress addrId)) Right
