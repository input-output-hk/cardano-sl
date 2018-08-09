module Cardano.Wallet.WalletLayer.Kernel.Addresses (
    createAddress
  , getAddresses
  , validateAddress
  ) where

import           Universum

import           Data.Coerce (coerce)

import           Pos.Core (Address)

import           Control.Monad.Trans.Except

import           Formatting (build, sformat, (%))


import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel

import           Cardano.Wallet.Kernel.Types (AccountId (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv
import           Cardano.Wallet.WalletLayer.Types (CreateAddressError (..))

import qualified Cardano.Wallet.Kernel.DB.BlockMeta as BlockMeta
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Read (readHdAccount,
                     readHdAddressByCardanoAddress)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.Read (hdWallets)
import           Cardano.Wallet.Kernel.DB.Spec (cpAddressMeta)
import           Cardano.Wallet.Kernel.Types (AccountId (..))
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Types (CreateAddressError (..),
                     ValidateAddressError (..))

import           Pos.Core (Address, decodeTextAddress)

import           Cardano.Wallet.API.Request (RequestParams (..))
import qualified Cardano.Wallet.API.V1.Types as V1

import           Cardano.Wallet.API.V1.Types (V1 (..))


createAddress :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.NewAddress
              -> m (Either CreateAddressError Address)
createAddress wallet
              (V1.NewAddress
                mbSpendingPassword
                accIx
                wId) = runExceptT $ do
    accId <- withExceptT CreateAddressAddressDecodingFailed $
               fromAccountId wId accIx
    withExceptT CreateAddressError $ ExceptT $ liftIO $
      Kernel.createAddress passPhrase (AccountIdHdRnd accId) wallet
  where
    passPhrase = maybe mempty coerce mbSpendingPassword

getAddresses :: RequestParams
             -> Kernel.DB
             -> [V1.WalletAddress]
getAddresses _params _db = error "todo"

validateAddress :: Text
                -- ^ A raw fragment of 'Text' to validate.
                -> Kernel.DB
                -> Either ValidateAddressError V1.WalletAddress
validateAddress rawText db =
    case decodeTextAddress rawText of
         Left _textualError -> Left (ValidateAddressDecodingFailed rawText)
         Right cardanoAddress ->
             case dbReads cardanoAddress of
               Left _dbErr  -> Left (ValidateAddressNotOurs cardanoAddress)
               Right (hdAccount, hdAddress) ->
                   -- At this point, we need to construct a full 'WalletAddress',
                   -- but we cannot do this without the knowledge on the
                   -- underlying associated 'HdAddress'.
                   Right $ toV1Address (hdAccount, hdAddress)
    where
    -- Reads both the 'HdAddress' associated to this Cardano 'Address' together
    -- its parent 'HdAccount'.
    dbReads :: HasCallStack
            => Address
            -> Either HD.UnknownHdAddress (HD.HdAccount, HD.HdAddress)
    dbReads cardanoAddress = runExcept $ do
        let hdAccId = HD.hdAddressId . HD.hdAddressIdParent
            q1 = readHdAddressByCardanoAddress cardanoAddress (hdWallets db)
            q2 hdAddr = readHdAccount (hdAddr ^. hdAccId) (hdWallets db)
            -- If we fail to read the associated account it really means our
            -- DB managed to become inconsistent and there is not much more
            -- we can do if not fail.
            hush hdAddr =
                let msg = "validateAddress: inconsistent DB, dangling " %
                          "address found with no associated account: " % build
                in error (sformat msg (hdAddr ^. HD.hdAddressId))
        hdAddr <- except q1
        hdAcc  <- withExcept (const (hush hdAddr)) $ except (q2 hdAddr)
        return (hdAcc, hdAddr)


toV1Address :: (HD.HdAccount, HD.HdAddress) -> V1.WalletAddress
toV1Address (hdAcc, hdAddr) =
    let address = hdAddr ^. HD.hdAddressAddress . fromDb
        checkpoint = hdAcc ^. HD.hdAccountState . HD.hdAccountStateCurrent
        addressMeta = checkpoint ^. cpAddressMeta address
        isUsed    = addressMeta ^. BlockMeta.addressMetaIsUsed
        isChange  = addressMeta ^. BlockMeta.addressMetaIsChange
    in V1.WalletAddress (V1 address) isUsed isChange
