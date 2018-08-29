{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.WIP.LegacyHandlers (
      handlers
    ) where

import           Formatting (build, sformat)
import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP
import           Pos.Chain.Update ()
import           Pos.Client.KeyStorage (addPublicKey)
import qualified Pos.Core as Core

import           Pos.Util (maybeThrow)
import           Pos.Util.Servant (encodeCType)
import qualified Pos.Wallet.WalletMode as V0
import           Servant

handlers :: HasConfigurations
            => (forall a. MonadV1 a -> Handler a)
            -> Server WIP.API
handlers naturalTransformation =
         hoist' (Proxy @WIP.API) handlersPlain
  where
    hoist'
        :: forall (api :: *). HasServer api '[]
        => Proxy api
        -> ServerT api MonadV1
        -> Server api
    hoist' p = hoistServer p naturalTransformation

-- | All the @Servant@ handlers for wallet-specific operations.
handlersPlain :: HasConfigurations
         => ServerT WIP.API MonadV1
handlersPlain = checkExternalWallet
    :<|> newExternalWallet
    :<|> deleteExternalWallet

-- | Check if external wallet is presented in node's wallet db.
checkExternalWallet
    :: -- ( V0.MonadWalletLogic ctx m
       -- , V0.MonadWalletHistory ctx m
       -- , MonadUnliftIO m
       -- , HasLens SyncQueue ctx SyncQueue
       -- )
       -- =>
    PublicKeyAsBase58
    -> m (WalletResponse WalletAndTxHistory)
checkExternalWallet _encodedRootPK =
    error "[CHW-54], Cardano Hardware Wallet, check external wallet, legacy handler, unimplemented yet."

-- | Creates a new or restores an existing external @wallet@ given a 'NewExternalWallet' payload.
-- Returns to the client the representation of the created or restored wallet in the 'Wallet' type.
newExternalWallet
    :: ( MonadThrow m
       , MonadUnliftIO m
       -- , HasLens SyncQueue ctx SyncQueue
       , V0.MonadBlockchainInfo m
       , V0.MonadWalletLogic ctx m
       )
    => NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet (NewExternalWallet rootPK assuranceLevel name operation) = do
    let newExternalWalletHandler CreateWallet  = createNewExternalWallet
        newExternalWalletHandler RestoreWallet = restoreExternalWallet
    walletMeta <- V0.CWalletMeta <$> pure name
                                 <*> migrate assuranceLevel
                                 <*> pure 0
    single <$> do
        v0wallet <- newExternalWalletHandler operation walletMeta rootPK
        ws <- V0.askWalletSnapshot
        migrateWallet ws v0wallet True

-- | Creates new external wallet.
createNewExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CWalletMeta
    -> PublicKeyAsBase58
    -> m V0.CWallet
createNewExternalWallet walletMeta encodedRootPK = do
    rootPK <- case mkPublicKeyFromBase58 encodedRootPK of
        Left problem -> throwM (InvalidPublicKey $ sformat build problem)
        Right rootPK -> return rootPK

    -- This extended public key will be used during synchronization
    -- with the blockchain.
    addPublicKey rootPK

    let walletId = encodeCType . Core.makePubKeyAddressBoot $ rootPK
        isReady  = True -- We don't need to sync new wallet with the blockchain.

    -- Create new external wallet.
    -- This is safe: if the client will try to create an external wallet from the same
    -- root public key - error will be thrown.
    void $ V0.createWalletSafe walletId walletMeta isReady

    addInitAccountInExternalWallet walletId

    V0.getWallet walletId

-- | Restore external wallet using it's root public key and metadata.
restoreExternalWallet
    :: -- ( MonadThrow m
       -- , MonadUnliftIO m
       -- , HasLens SyncQueue ctx SyncQueue
       -- , V0.MonadWalletLogic ctx m
       -- )
       -- =>
    V0.CWalletMeta
    -> PublicKeyAsBase58
    -> m V0.CWallet
restoreExternalWallet _walletMeta _encodedRootPK =
    error "[CHW-54], restore external wallet, unimplemented yet."

addInitAccountInExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CId V0.Wal
    -> m ()
addInitAccountInExternalWallet walletId = do
    let accountName = "Initial account"
        accountMeta = V0.CAccountMeta accountName
        accountInit = V0.CAccountInit accountMeta walletId
        includeUnready = True
    void $ V0.newExternalAccountIncludeUnready includeUnready accountInit

-- | On the disk, once imported or created, there's so far not much difference
-- between a wallet and an external wallet, except one: node stores a public key
-- for external wallet, there's no secret key.
deleteExternalWallet
    :: (V0.MonadWalletLogic ctx m)
    => PublicKeyAsBase58
    -> m NoContent
deleteExternalWallet encodedRootPK =
    case V1.mkPublicKeyFromBase58 encodedRootPK of
        Left problem -> throwM (InvalidPublicKey $ sformat build problem)
        Right rootPK -> V0.deleteExternalWallet rootPK

migrateWallet
    :: ( V0.MonadWalletLogicRead ctx m
       , V0.MonadBlockchainInfo m
       )
    => V0.WalletSnapshot
    -> V0.CWallet
    -> Bool
    -> m Wallet
migrateWallet snapshot wallet walletIsReady = do
    let walletId = V0.cwId wallet
    walletInfo <- if walletIsReady
        then maybeThrow WalletNotFound $ V0.getWalletInfo walletId snapshot
        else
            -- Wallet is not ready yet (because of restoring),
            -- the only information we can provide is the default one.
            pure $ V0.getUnreadyWalletInfo snapshot
    walletIsExternal <- V0.isWalletExternal walletId
    let walletType = if walletIsExternal then WalletExternal else WalletRegular
    currentDepth <- V0.networkChainDifficulty
    migrate (wallet, walletInfo, walletType, currentDepth)
