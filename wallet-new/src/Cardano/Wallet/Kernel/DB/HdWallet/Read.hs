-- | READ queries on the HD wallet
--
-- NOTE: These are pure functions, which are intended to work on a snapshot
-- of the database. They are intended to support the V1 wallet API.
--
-- Filtering and sorting will be the responsibility of the next layer up.
module Cardano.Wallet.Kernel.DB.HdWallet.Read (
    -- | All wallets/accounts/addresses
    readHdRoots
  , readHdAccounts
  , readHdAddresses
    -- | Accumulate all accounts/addresses
  , readAllHdAccounts
  , readAllHdAddresses
    -- | Single wallets/accounts/addresses
  , readHdRoot
  , readHdAccount
  , readHdAddress
  ) where

import           Universum

import qualified Data.Map as Map

import           Pos.Core (Coin)

import           Cardano.Wallet.Kernel.DB.HdWallet

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

-- | Query on a HD wallet
type HdQuery a = HdRoots -> a

-- | Like 'HdQuery', but with the possibility of errors
type HdQueryErr e a = HdQuery (Either e a)

{-------------------------------------------------------------------------------
  Internal functions
-------------------------------------------------------------------------------}

hdRootsInfo :: HdRoots -> Map HdRootId (HdRoot, Integer)
hdRootsInfo =
    map aux
  where
    aux :: HdRoot -> (HdRoot, Integer)
    aux hdRoot = (hdRoot, hdRootBalance hdRoot)

hdAccountsInfo :: HdRootId -> HdRoot -> Map HdAccountId (HdAccount, Coin)
hdAccountsInfo rootId =
    mapKeysVals onKeys onVals . view hdRootAccounts
  where
    onKeys :: AccountIx -> HdAccountId
    onKeys = HdAccountId rootId

    onVals :: HdAccount -> (HdAccount, Coin)
    onVals hdAccount = (hdAccount, hdAccountBalance hdAccount)

hdAddressesInfo :: HdAccountId -> HdAccount -> Map HdAddressId HdAddress
hdAddressesInfo accId =
    Map.mapKeysMonotonic onKeys . view hdAccountAddresses
  where
    onKeys :: AddressIx -> HdAddressId
    onKeys = HdAddressId accId

{-------------------------------------------------------------------------------
  Information about all wallets/accounts/addresses
-------------------------------------------------------------------------------}

-- | Meta information and total balance of all wallets
readHdRoots :: HdQuery (Map HdRootId (HdRoot, Integer))
readHdRoots = hdRootsInfo

-- | Meta-information and total balance of all accounts in the given wallet
readHdAccounts :: HdRootId
               -> HdQueryErr UnknownHdRoot (Map HdAccountId (HdAccount, Coin))
readHdAccounts rootId =
      fmap (hdAccountsInfo rootId . fst)
    . readHdRoot rootId

-- | Meta-information about the addresses in an account
readHdAddresses :: HdAccountId
                -> HdQueryErr UnknownHdAccount (Map HdAddressId HdAddress)
readHdAddresses accId =
      fmap (hdAddressesInfo accId . fst)
    . readHdAccount accId

{-------------------------------------------------------------------------------
  Accumulate across wallets/accounts
-------------------------------------------------------------------------------}

-- | Meta-information and total balance of /all/ accounts
readAllHdAccounts :: HdQuery (Map HdAccountId (HdAccount, Coin))
readAllHdAccounts =
      Map.unions
    . map (uncurry hdAccountsInfo)
    . Map.toList . map fst
    . readHdRoots

-- | Meta-information and total balance of /all/ addresses
readAllHdAddresses :: HdQuery (Map HdAddressId HdAddress)
readAllHdAddresses =
      Map.unions
    . map (uncurry hdAddressesInfo)
    . Map.toList . map fst
    . readAllHdAccounts

{-------------------------------------------------------------------------------
  Information about a single wallet/address/account
-------------------------------------------------------------------------------}

-- | Meta information and total balance of the given wallet
readHdRoot :: HdRootId -> HdQueryErr UnknownHdRoot (HdRoot, Integer)
readHdRoot rootId =
      aux . Map.lookup rootId
    . readHdRoots
  where
    aux :: Maybe a -> Either UnknownHdRoot a
    aux = maybe (Left (UnknownHdRoot rootId)) Right

-- | Meta-information and balance of the given account
readHdAccount :: HdAccountId -> HdQueryErr UnknownHdAccount (HdAccount, Coin)
readHdAccount accId@(HdAccountId rootId _) =
      either (Left . embedUnknownHdRoot) (aux . Map.lookup accId)
    . readHdAccounts rootId
  where
    aux :: Maybe a -> Either UnknownHdAccount a
    aux = maybe (Left (UnknownHdAccount accId)) Right

-- | Meta-information about an address
--
-- We do NOT compute or cache a per-address balance.
readHdAddress :: HdAddressId -> HdQueryErr UnknownHdAddress HdAddress
readHdAddress addrId@(HdAddressId accId _) =
      either (Left . embedUnknownHdAccount) (aux . Map.lookup addrId)
    . readHdAddresses accId
  where
    aux :: Maybe a -> Either UnknownHdAddress a
    aux = maybe (Left (UnknownHdAddress addrId)) Right

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Map both the keys and the values of a map
--
-- NOTE: Uses 'mapKeysMonotonic'; see side conditions.
mapKeysVals :: (k1 -> k2) -> (a -> b) -> Map k1 a -> Map k2 b
mapKeysVals onKeys onVals = map onVals . Map.mapKeysMonotonic onKeys
