{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Legacy
    ( MonadLegacyWallet
    , bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Control.Monad.Catch (catchAll)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Coerce (coerce)

import           Cardano.Wallet.WalletLayer.Error (WalletLayerError (..))
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.API.V1.Migration (migrate)
import           Cardano.Wallet.API.V1.Migration.Types ()
import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, AccountUpdate, Address,
                                              NewAccount (..), NewWallet (..), V1 (..), Wallet,
                                              WalletId, WalletOperation (..), WalletUpdate)

import           Pos.Client.KeyStorage (MonadKeys)
import           Pos.Core (ChainDifficulty)
import           Pos.Crypto (PassPhrase)

import           Pos.Util (HasLens', maybeThrow)
import           Pos.Wallet.Web.Account (GenSeed (..))
import           Pos.Wallet.Web.ClientTypes.Types (CWallet (..), CWalletInit (..), CWalletMeta (..))
import qualified Pos.Wallet.Web.Error.Types as V0
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogicRead)
import qualified Pos.Wallet.Web.Methods.Logic as V0
import           Pos.Wallet.Web.Methods.Restore (newWallet, restoreWalletFromSeed)
import           Pos.Wallet.Web.State.State (WalletDbReader, askWalletDB, askWalletSnapshot,
                                             getWalletAddresses, setWalletMeta)
import           Pos.Wallet.Web.State.Storage (getWalletInfo)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)

import           Pos.Util.Chrono (NE, OldestFirst (..), NewestFirst (..))
import           Pos.Block.Types (Blund)


-- | Let's unify all the requirements for the legacy wallet.
type MonadLegacyWallet ctx m =
    ( WalletDbReader ctx m
    , HasLens' ctx SyncQueue
    , MonadUnliftIO m
    , MonadIO m
    , MonadThrow m
    , MonadWalletLogicRead ctx m
    , MonadKeys m
    )

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall ctx m n a. (MonadMask n, MonadLegacyWallet ctx m)
    => (PassiveWalletLayer m -> n a) -> n a
bracketPassiveWallet =
    bracket
        (pure passiveWalletLayer)
        (\_ -> return ())
  where
    passiveWalletLayer :: PassiveWalletLayer m
    passiveWalletLayer = PassiveWalletLayer
        { _pwlCreateWallet   = pwlCreateWallet
        , _pwlGetWalletIds   = pwlGetWalletIds
        , _pwlGetWallet      = pwlGetWallet
        , _pwlUpdateWallet   = pwlUpdateWallet
        , _pwlDeleteWallet   = pwlDeleteWallet

        , _pwlCreateAccount  = pwlCreateAccount
        , _pwlGetAccounts    = pwlGetAccounts
        , _pwlGetAccount     = pwlGetAccount
        , _pwlUpdateAccount  = pwlUpdateAccount
        , _pwlDeleteAccount  = pwlDeleteAccount

        , _pwlGetAddresses   = pwlGetAddresses

        , _pwlApplyBlocks    = pwlApplyBlocks
        , _pwlRollbackBlocks = pwlRollbackBlocks
        }


-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall ctx m n a. (MonadMask n, MonadLegacyWallet ctx m)
    => PassiveWalletLayer m
    -> WalletDiffusion
    -> (ActiveWalletLayer m -> n a) -> n a
bracketActiveWallet walletPassiveLayer _walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())

------------------------------------------------------------
-- Wallet
------------------------------------------------------------

pwlCreateWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => NewWallet
    -> m Wallet
pwlCreateWallet NewWallet{..} = do

    let spendingPassword = fromMaybe mempty $ coerce newwalSpendingPassword
    let backupPhrase     = coerce newwalBackupPhrase

    initMeta    <- CWalletMeta  <$> pure newwalName
                                <*> migrate newwalAssuranceLevel
                                <*> pure 0

    let walletInit = CWalletInit initMeta backupPhrase

    wallet      <- newWalletHandler newwalOperation spendingPassword walletInit
                       `catch` rethrowDuplicateMnemonic
    wId         <- migrate $ cwId wallet

    -- Get wallet or throw if missing.
    maybeThrow (WalletNotFound wId) =<< pwlGetWallet wId
  where
    -- | We have two functions which are very similar.
    newWalletHandler :: WalletOperation -> PassPhrase -> CWalletInit -> m CWallet
    newWalletHandler CreateWallet  = newWallet
    newWalletHandler RestoreWallet = restoreWalletFromSeed
    -- NOTE: this is temporary solution until we get rid of V0 error handling and/or we lift error handling into types:
    --   https://github.com/input-output-hk/cardano-sl/pull/2811#discussion_r183469153
    --   https://github.com/input-output-hk/cardano-sl/pull/2811#discussion_r183472103
    rethrowDuplicateMnemonic (e :: V0.WalletError) =
        case e of
            V0.RequestError "Wallet with that mnemonics already exists" -> throwM WalletAlreadyExists
            _ -> throwM e


pwlGetWalletIds
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => m [WalletId]
pwlGetWalletIds = do
    ws          <- askWalletSnapshot
    migrate $ getWalletAddresses ws

pwlGetWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m (Maybe Wallet)
pwlGetWallet wId = do
    ws          <- askWalletSnapshot

    cWId        <- migrate wId
    wallet      <- V0.getWallet cWId

    pure $ do
        walletInfo  <- getWalletInfo cWId ws
        migrate (wallet, walletInfo, Nothing @ChainDifficulty)

--instance Migrate (V0.CWallet, OldStorage.WalletInfo, Maybe Core.ChainDifficulty) V1.Wallet where

pwlUpdateWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> WalletUpdate
    -> m Wallet
pwlUpdateWallet wId wUpdate = do
    walletDB    <- askWalletDB

    cWId        <- migrate wId
    cWMeta      <- migrate wUpdate

    -- Update the data
    setWalletMeta walletDB cWId cWMeta

    -- Get wallet or throw if missing.
    maybeThrow (WalletNotFound wId) =<< pwlGetWallet wId

-- | Seems silly, but we do need some sort of feedback from
-- the DB.
pwlDeleteWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m Bool
pwlDeleteWallet wId = do
    cWId        <- migrate wId
    -- TODO(ks): It would be better to catch specific @Exception@.
    -- Maybe @try@?
    catchAll (const True <$> V0.deleteWallet cWId) (const . pure $ False)

------------------------------------------------------------
-- Account
------------------------------------------------------------

pwlCreateAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> NewAccount
    -> m Account
pwlCreateAccount wId newAcc@NewAccount{..} = do

    let spendingPassword = fromMaybe mempty . fmap coerce $ naccSpendingPassword

    accInit     <- migrate (wId, newAcc)
    cAccount    <- V0.newAccount RandomSeed spendingPassword accInit

    migrate cAccount

pwlGetAccounts
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m [Account]
pwlGetAccounts wId = do
    cWId        <- migrate wId
    cAccounts   <- V0.getAccounts $ Just cWId
    migrate cAccounts

pwlGetAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> m (Maybe Account)
pwlGetAccount wId aId = do
    accId       <- migrate (wId, aId)
    account     <- V0.getAccount accId
    Just <$> migrate account

pwlUpdateAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> AccountUpdate
    -> m Account
pwlUpdateAccount wId accIdx accUpdate = do
    newAccId    <- migrate (wId, accIdx)
    accMeta     <- migrate accUpdate
    cAccount    <- V0.updateAccount newAccId accMeta
    migrate cAccount

pwlDeleteAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> m Bool
pwlDeleteAccount wId accIdx = do
    accId <- migrate (wId, accIdx)
    catchAll (const True <$> V0.deleteAccount accId) (const . pure $ False)

------------------------------------------------------------
-- Address
------------------------------------------------------------

pwlGetAddresses :: WalletId -> m [Address]
pwlGetAddresses = error "Not implemented!"

------------------------------------------------------------
-- Apply Block
------------------------------------------------------------

pwlApplyBlocks :: OldestFirst NE Blund -> m ()
pwlApplyBlocks = error "Not implemented!"

------------------------------------------------------------
-- Rollback Block
------------------------------------------------------------

pwlRollbackBlocks :: NewestFirst NE Blund -> m ()
pwlRollbackBlocks = error "Not implemented!"
