{-# LANGUAGE ViewPatterns #-}
module Cardano.Wallet.WalletLayer.Kernel.Addresses (
    createAddress
  , getAddresses
  , validateAddress
  ) where

import           Universum

import           Control.Monad.Trans.Except
import           Data.Coerce (coerce)

import           Pos.Core (Address, decodeTextAddress)

import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Pagination (Page (..),
                     PaginationParams (..), PerPage (..))
import           Cardano.Wallet.API.Response (SliceOf (..))
import           Cardano.Wallet.API.V1.Types (V1 (..), WalletAddress (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (dbHdWallets)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.Util.IxSet (AutoIncrementKey (..),
                     Indexed (..), IxSet, ixedIndexed, (@>=<=))
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Types (AccountId (..))
import           Cardano.Wallet.WalletLayer (CreateAddressError (..),
                     ValidateAddressError (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv

createAddress :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.NewAddress
              -> m (Either CreateAddressError WalletAddress)
createAddress wallet
              (V1.NewAddress
                mbSpendingPassword
                accIx
                wId) = runExceptT $ do
    accId <- withExceptT CreateAddressAddressDecodingFailed $
               fromAccountId wId accIx
    fmap mkAddress $
        withExceptT CreateAddressError $ ExceptT $ liftIO $
            Kernel.createAddress passPhrase (AccountIdHdRnd accId) wallet
  where
    passPhrase = maybe mempty coerce mbSpendingPassword

    -- | Creates a new 'WalletAddress'. As this is a brand new, fresh Address,
    -- it's fine to have 'False' for both 'isUsed' and 'isChange'.
    mkAddress :: Address -> WalletAddress
    mkAddress addr = WalletAddress (V1 addr) False False (V1 V1.AddressIsOurs)



-- | Get all the addresses known to this node. Doing this efficiently is
-- a bit of a challenge here due to the fact we don't have a way with IxSets to
-- \"jump\" in the middle of a collection and extracts only the needed
-- data. Luckily, we can still associate an auto-incrementing index and
-- use IxSet's range operators to extract data intervals. However, due to the
-- fact we store @all@ our addresses in a single IxSet collection, having a
-- @global@ counter for such indices won't work, as we can delete addresses
-- (when we delete an account) leaving \"holes\" in our collection.
--
-- +-----+-----+----------------------------------------------+------+
-- | A1  | A2  | ...                                          | A100 |
-- |     |     |                                              |      |
-- +-----+-----+----------------------------------------------+------+
--    1     2                       ..                          100
-- <deletion of an account happens, leaving a hole in the indices>
--
--                        /              /
-- +-----+-----+---------/--------------/---------------------+------+
-- | A1  | A2  | ...    X              X                      | A100 |
-- |     |     |         \              \                     |      |
-- +-----+-----+----------\--------------\--------------------+------+
--    1     2    ..    10  \              \ 30      ..          100
--                          o              \
--
-- What we can do, instead, is to have a @local@ counter associated to each
-- account in the system, so that we can perform simple arithmetic to map from
-- global pagination parameters into local ones.
--
-- +-----+-----+--------+------+-----+-----+--------+-----+-----+------+
-- | A1  | A2  | ...    | A10  | A11 | A12 |  ...   | A98 | A99 | A100 |
-- |     |     |        |      |     |     |        |     |     |      |
-- +-----+-----+--------+------+-----+-----+--------+-----+-----+------+
--    1     2              1      2     3              1     2     3
-- |--- Account 1 -----||----- Account 2 ----------||---- Account 3 ---|
--
getAddresses :: RequestParams
             -> Kernel.DB
             -> SliceOf V1.WalletAddress
getAddresses (RequestParams (PaginationParams (Page p) (PerPage pp))) db =
   let allAddrs = db ^. dbHdWallets . HD.hdWalletsAddresses
       slice :: [V1.WalletAddress]
       slice | p < 1 = []
             | pp < 1 = []
             | otherwise = let globalStart = (p - 1) * pp
                           in findResults db globalStart pp
   in SliceOf slice (IxSet.size allAddrs)

-- | Finds the results needed by 'getAddresses'.
findResults :: Kernel.DB
            -> Int
            -> Int
            -> [V1.WalletAddress]
findResults db globalStartIndex howMany =
    let -- The number of accounts is small enough to justify this conversion
        accounts  = IxSet.toList $ db ^. dbHdWallets . HD.hdWalletsAccounts
    in takeIndexed db howMany [] . dropIndexed db globalStartIndex $ accounts


-- | An 'Ordering' function over the 'AutoIncrementKey' of this 'Indexed'
-- resource.
autoKey :: Indexed a -> Indexed a -> Ordering
autoKey (Indexed ix1 _) (Indexed ix2 _) = compare ix1 ix2


dropIndexed :: Kernel.DB -> Int -> [HD.HdAccount] -> (Int, [HD.HdAccount])
dropIndexed _  n []     = (n, [])
dropIndexed db n (a:as) =
    let addresses = readAddresses db a
        addrSize  = IxSet.size addresses
    in if addrSize < n
          then dropIndexed db (n - addrSize) as
          else (n, a:as)


takeIndexed :: Kernel.DB
            -> Int
            -> [V1.WalletAddress]
            -> (Int, [HD.HdAccount])
            -> [V1.WalletAddress]
takeIndexed _ _ acc (_, []) = acc
takeIndexed db n acc (currentIndex, (a:as))
    | n < 0 = error "takeIndexed: invariant violated, n is negative."
    | n == 0 = acc
    | otherwise =
        let addresses = readAddresses db a
            addrSize  = IxSet.size addresses
            interval = ( AutoIncrementKey currentIndex
                       , AutoIncrementKey $
                           min (currentIndex + n - 1) (addrSize - 1)
                       )
            slice = addresses @>=<= interval
            new = map (toV1 a) . sortBy autoKey . IxSet.toList $ slice
            in  -- For the next iterations, the index will always be 0 as we
                -- are hopping from one ixset to the other, collecting addresses.
                takeIndexed db (n - IxSet.size slice) (acc <> new) (0, as)
  where
    toV1 :: HD.HdAccount -> Indexed HD.HdAddress -> V1.WalletAddress
    toV1 hdAccount ixed = toAddress hdAccount (ixed ^. ixedIndexed)

readAddresses :: Kernel.DB -> HD.HdAccount -> IxSet (Indexed HD.HdAddress)
readAddresses db hdAccount = Kernel.addressesByAccountId db (hdAccount ^. HD.hdAccountId)

-- | Validate an address
validateAddress :: Text
                -- ^ A raw fragment of 'Text' to validate.
                -> Kernel.DB
                -> Either ValidateAddressError V1.WalletAddress
validateAddress rawText db = runExcept $ do
    cardanoAddress <- withExceptT (\_err -> ValidateAddressDecodingFailed rawText) $
                        exceptT $ decodeTextAddress rawText
    let mAddr = runExcept $ do
          addr <- exceptT $
                    Kernel.lookupCardanoAddress db
                      cardanoAddress
          acc  <- withExceptT HD.embedUnknownHdAccount $ exceptT $
                    Kernel.lookupHdAccountId db
                      (addr ^. HD.hdAddressId . HD.hdAddressIdParent)
          return (addr, acc)
    case mAddr of
      Right (hdAddress, hdAccount) -> return $ toAddress hdAccount hdAddress
      Left _unknownHdAddr ->
        -- If the address is unknown to the wallet, it's possible that it's ours
        -- but not yet used (at least as far as we know, it may be pending in
        -- another instance of the same wallet of course), or it may be that
        -- it's not even ours. In both cases we return 'V1.AddressAmbiguousOwnership'.
        return V1.WalletAddress {
            addrId            = V1 cardanoAddress
          , addrUsed          = False
          , addrChangeAddress = False
          , addrOwnership     = (V1 V1.AddressAmbiguousOwnership)
          }
