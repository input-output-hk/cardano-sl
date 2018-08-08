module Cardano.Wallet.WalletLayer.Kernel.Wallets (
      createWallet
    , updateWallet
    , updateWalletPassword
    , deleteWallet
    , getWallet
    , getWallets
    ) where

import           Universum

import           Control.Lens (to)
import           Data.Coerce (coerce)
import           Data.Time.Units (Second)
import           Formatting (build, sformat)

import           Pos.Core (Coin, decodeTextAddress, mkCoin, unsafeAddCoin)
import           Pos.Crypto.Signing

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Read (readAllHdRoots,
                     readHdRoot)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.Read (hdWallets)
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core (getCurrentTimestamp)
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Types (CreateWalletError (..),
                     DeleteWalletError (..), GetWalletError (..),
                     UpdateWalletError (..), UpdateWalletPasswordError (..))

createWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.NewWallet
             -> m (Either CreateWalletError V1.Wallet)
createWallet wallet (V1.NewWallet (V1.BackupPhrase mnemonic) mbSpendingPassword v1AssuranceLevel v1WalletName operation) = do
    liftIO $ limitExecutionTimeTo (30 :: Second) CreateWalletTimeLimitReached $ do
        case operation of
             V1.RestoreWallet -> error "Not implemented, see [CBR-243]."
             V1.CreateWallet  -> do
                 let spendingPassword = maybe emptyPassphrase coerce mbSpendingPassword
                 let hdAssuranceLevel = case v1AssuranceLevel of
                       V1.NormalAssurance -> HD.AssuranceLevelNormal
                       V1.StrictAssurance -> HD.AssuranceLevelStrict

                 res <- liftIO $ Kernel.createHdWallet wallet
                                                       mnemonic
                                                       spendingPassword
                                                       hdAssuranceLevel
                                                       (HD.WalletName v1WalletName)
                 case res of
                      Left kernelError ->
                          return (Left $ CreateWalletError kernelError)
                      Right hdRoot -> do
                          let rootId = hdRoot ^. HD.hdRootId
                          -- Populate this wallet with an account by default
                          newAccount <- liftIO $ Kernel.createAccount spendingPassword
                                                                      (HD.AccountName "Default account")
                                                                      (WalletIdHdRnd rootId)
                                                                      wallet
                          case newAccount of
                               Left accCreationFailed ->
                                   return (Left $ CreateWalletFirstAccountCreationFailed accCreationFailed)
                               Right _ -> do
                                   let (hasSpendingPassword, mbLastUpdate) =
                                           case hdRoot ^. HD.hdRootHasPassword of
                                                HD.NoSpendingPassword -> (False, Nothing)
                                                HD.HasSpendingPassword lu -> (True, Just (lu ^. fromDb))
                                   now <- liftIO getCurrentTimestamp
                                   let lastUpdate = fromMaybe now mbLastUpdate
                                   let createdAt  = hdRoot ^. HD.hdRootCreatedAt . fromDb
                                   let walletId = hdRoot ^. HD.hdRootId . to (sformat build . _fromDb . HD.getHdRootId)
                                   return $ Right V1.Wallet {
                                       walId                         = (V1.WalletId walletId)
                                     , walName                       = v1WalletName
                                     , walBalance                    = V1 (mkCoin 0)
                                     , walHasSpendingPassword        = hasSpendingPassword
                                     , walSpendingPasswordLastUpdate = V1 lastUpdate
                                     , walCreatedAt                  = V1 createdAt
                                     , walAssuranceLevel             = v1AssuranceLevel
                                     , walSyncState                  = V1.Synced
                                   }

-- | Updates the 'SpendingPassword' for this wallet.
updateWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> V1.WalletUpdate
             -> m (Either UpdateWalletError V1.Wallet)
updateWallet wallet (V1.WalletId wId) (V1.WalletUpdate v1Level v1Name) = do
    case decodeTextAddress wId of
        Left _ -> return $ Left (UpdateWalletWalletIdDecodingFailed wId)
        Right rootAddr -> do
           let hdRootId = HD.HdRootId . InDb $ rootAddr
               newLevel = fromV1AssuranceLevel v1Level
               newName  = HD.WalletName v1Name
           res <- liftIO $ Kernel.updateHdWallet wallet hdRootId newLevel newName
           case res of
                Left e  -> return $ Left (UpdateWalletError (V1 e))
                Right (db, updatedWallet) ->
                    return $ Right $ toV1Wallet db updatedWallet

-- | Updates the 'SpendingPassword' for this wallet.
updateWalletPassword :: MonadIO m
                     => Kernel.PassiveWallet
                     -> V1.WalletId
                     -> V1.PasswordUpdate
                     -> m (Either UpdateWalletPasswordError V1.Wallet)
updateWalletPassword wallet (V1.WalletId wId) (V1.PasswordUpdate (V1 oldPwd) (V1 newPwd)) =
    case decodeTextAddress wId of
        Left _ -> return $ Left (UpdateWalletPasswordWalletIdDecodingFailed wId)
        Right rootAddr -> do
           let hdRootId = HD.HdRootId . InDb $ rootAddr
           res <- liftIO $ Kernel.updatePassword wallet hdRootId oldPwd newPwd
           case res of
                Left e  -> return $ Left (UpdateWalletPasswordError e)
                Right (db, updatedWallet) ->
                    return $ Right $ toV1Wallet db updatedWallet

-- | Updates the 'SpendingPassword' for this wallet.
deleteWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> m (Either DeleteWalletError ())
deleteWallet wallet (V1.WalletId wId) =
    case decodeTextAddress wId of
        Left _ -> return $ Left (DeleteWalletWalletIdDecodingFailed wId)
        Right rootAddr -> do
           let hdRootId = HD.HdRootId . InDb $ rootAddr
           res <- liftIO $ Kernel.deleteHdWallet wallet hdRootId
           case res of
                Left e   -> return $ Left (DeleteWalletError (V1 e))
                Right () -> return $ Right ()

-- | Gets a specific wallet.
getWallet :: Kernel.DB
          -> V1.WalletId
          -> Either GetWalletError V1.Wallet
getWallet db (V1.WalletId wId) =
    case decodeTextAddress wId of
        Left _ -> Left (GetWalletWalletIdDecodingFailed wId)
        Right rootAddr -> do
           let hdRootId = HD.HdRootId . InDb $ rootAddr
           case readHdRoot hdRootId (hdWallets db) of
                Left dbErr -> Left (GetWalletError (V1 dbErr))
                Right w    -> Right (toV1Wallet db w)

-- | Gets all the wallets known to this edge node.
getWallets :: Kernel.DB -> IxSet V1.Wallet
getWallets db =
    let allRoots = readAllHdRoots (hdWallets db)
    in IxSet.fromList . map (toV1Wallet db) . IxSet.toList $ allRoots

{------------------------------------------------------------------------------
  General utility functions on the wallets.
------------------------------------------------------------------------------}

-- | Converts an 'HdRoot' into a V1 'Wallet.
toV1Wallet :: Kernel.DB -> HD.HdRoot -> V1.Wallet
toV1Wallet db hdRoot =
    let (hasSpendingPassword, mbLastUpdate) =
            case hdRoot ^. HD.hdRootHasPassword of
                 HD.NoSpendingPassword     -> (False, Nothing)
                 HD.HasSpendingPassword lu -> (True, Just (lu ^. fromDb))
        -- In case the wallet has no spending password, its last update
        -- matches this wallet creation time.
        rootId = hdRoot ^. HD.hdRootId
        createdAt  = hdRoot ^. HD.hdRootCreatedAt . fromDb
        lastUpdate = fromMaybe createdAt mbLastUpdate
        walletId   = sformat build . _fromDb . HD.getHdRootId $ rootId
        v1AssuranceLevel = case hdRoot ^. HD.hdRootAssurance of
                               HD.AssuranceLevelNormal -> V1.NormalAssurance
                               HD.AssuranceLevelStrict -> V1.StrictAssurance
    in V1.Wallet {
        walId                         = (V1.WalletId walletId)
      , walName                       = hdRoot ^. HD.hdRootName
                                                . to HD.getWalletName
      , walBalance                    = V1 (walletTotalBalance db rootId)
      , walHasSpendingPassword        = hasSpendingPassword
      , walSpendingPasswordLastUpdate = V1 lastUpdate
      , walCreatedAt                  = V1 createdAt
      , walAssuranceLevel             = v1AssuranceLevel
      -- FIXME(adn) Do this as part of CBR-243.
      , walSyncState                  = V1.Synced
    }

-- | Converts from the @V1@ 'AssuranceLevel' to the HD one.
fromV1AssuranceLevel :: V1.AssuranceLevel -> HD.AssuranceLevel
fromV1AssuranceLevel V1.NormalAssurance = HD.AssuranceLevelNormal
fromV1AssuranceLevel V1.StrictAssurance = HD.AssuranceLevelStrict

-- | Computes the total balance for this wallet, given its 'HdRootId'.
walletTotalBalance :: Kernel.DB -> HD.HdRootId -> Coin
walletTotalBalance db hdRootId =
    IxSet.foldl' (\total account ->
                      total `unsafeAddCoin`
                      Kernel.accountTotalBalance db (account ^. HD.hdAccountId)
                 )
                 (mkCoin 0)
                 (Kernel.walletAccounts db hdRootId)
