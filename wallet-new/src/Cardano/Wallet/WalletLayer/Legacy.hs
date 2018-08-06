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
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..),
                     CreateAccountError (..), CreateAddressError (..),
                     CreateWalletError, DeleteAccountError, GetAccountError,
                     GetAccountsError, PassiveWalletLayer (..),
                     UpdateAccountError)

import           Cardano.Wallet.API.V1.Migration (migrate)
import           Cardano.Wallet.API.V1.Migration.Types ()
import           Cardano.Wallet.API.V1.Types (Account, AccountIndex,
                     AccountUpdate, Address, BackupPhrase (..),
                     NewAccount (..), NewAddress, NewWallet (..), V1 (..),
                     Wallet, WalletId, WalletOperation (..), WalletUpdate)
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import           Pos.Client.KeyStorage (MonadKeys)
import           Pos.Core (ChainDifficulty)
import           Pos.Crypto (PassPhrase)

import           Pos.Util (HasLens', maybeThrow)
import           Pos.Util.Trace.Named (TraceNamed)
import           Pos.Wallet.Web.Account (GenSeed (..))
import           Pos.Wallet.Web.ClientTypes.Types (CBackupPhrase (..),
                     CWallet (..), CWalletInit (..), CWalletMeta (..))
import qualified Pos.Wallet.Web.Error.Types as V0
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogicRead)
import qualified Pos.Wallet.Web.Methods.Logic as V0
import           Pos.Wallet.Web.Methods.Restore (newWallet,
                     restoreWalletFromSeed)
import           Pos.Wallet.Web.State.State (WalletDbReader, askWalletDB,
                     askWalletSnapshot, getWalletAddresses, setWalletMeta)
import           Pos.Wallet.Web.State.Storage (getWalletInfo)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)

import           Pos.Chain.Block (Blund)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))


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
    => TraceNamed m
    -> (PassiveWalletLayer m -> n a) -> n a
bracketPassiveWallet logTrace =
    bracket
        (pure passiveWalletLayer)
        (\_ -> return ())
  where
    passiveWalletLayer :: PassiveWalletLayer m
    passiveWalletLayer = PassiveWalletLayer
        { _pwlCreateWallet   = pwlCreateWallet logTrace
        , _pwlGetWalletIds   = pwlGetWalletIds
        , _pwlGetWallet      = pwlGetWallet logTrace
        , _pwlUpdateWallet   = pwlUpdateWallet logTrace
        , _pwlDeleteWallet   = pwlDeleteWallet

        , _pwlCreateAccount  = pwlCreateAccount logTrace
        , _pwlGetAccounts    = pwlGetAccounts logTrace
        , _pwlGetAccount     = pwlGetAccount logTrace
        , _pwlUpdateAccount  = pwlUpdateAccount logTrace
        , _pwlDeleteAccount  = pwlDeleteAccount

        , _pwlCreateAddress  = pwlCreateAddress
        , _pwlGetAddresses   = pwlGetAddresses

        , _pwlApplyBlocks    = pwlApplyBlocks
        , _pwlRollbackBlocks = pwlRollbackBlocks
        }


-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadMask n)
    => PassiveWalletLayer m
    -> WalletDiffusion
    -> (ActiveWalletLayer m -> n a) -> n a
bracketActiveWallet walletPassiveLayer _walletDiffusion =
    bracket
      (return activeWalletLayer)
      (\_ -> return ())
  where
    notUsed = "The activeWalletLayer is not used for the legacy handlers, " <>
              "for historical reasons. However, if you wish to use it, simply " <>
              "move the concrete implementation of your LegacyHandler(s) into " <>
              "this function."
    activeWalletLayer :: ActiveWalletLayer m
    activeWalletLayer = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer
        , pay          = \_ _ _ -> error notUsed
        , estimateFees = \_ _ _ -> error notUsed
        }

------------------------------------------------------------
-- Wallet
------------------------------------------------------------

pwlCreateWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => TraceNamed m
    -> NewWallet
    -> m (Either CreateWalletError Wallet)
pwlCreateWallet logTrace NewWallet{..} = do
    let spendingPassword = fromMaybe mempty $ coerce newwalSpendingPassword
    let backupPhrase     = CBackupPhrase $ unBackupPhrase newwalBackupPhrase

    initMeta    <- CWalletMeta  <$> pure newwalName
                                <*> migrate newwalAssuranceLevel
                                <*> pure 0

    let walletInit = CWalletInit initMeta backupPhrase

    wallet      <- newWalletHandler newwalOperation spendingPassword walletInit
                       `catch` rethrowDuplicateMnemonic
    wId         <- migrate $ cwId wallet

    -- Get wallet or throw if missing.
    w <- maybeThrow (WalletNotFound wId) =<< pwlGetWallet logTrace wId
    return $ Right w
  where
    -- | We have two functions which are very similar.
    newWalletHandler :: WalletOperation -> PassPhrase -> CWalletInit -> m CWallet
    newWalletHandler CreateWallet  = newWallet logTrace
    newWalletHandler RestoreWallet = restoreWalletFromSeed logTrace
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
    => TraceNamed m
    -> WalletId
    -> m (Maybe Wallet)
pwlGetWallet logTrace wId = do
    ws          <- askWalletSnapshot

    cWId        <- migrate wId
    wallet      <- V0.getWallet logTrace cWId

    pure $ do
        walletInfo  <- getWalletInfo cWId ws
        migrate (wallet, walletInfo, Nothing @ChainDifficulty)

--instance Migrate (V0.CWallet, OldStorage.WalletInfo, Maybe Core.ChainDifficulty) V1.Wallet where

pwlUpdateWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => TraceNamed m
    -> WalletId
    -> WalletUpdate
    -> m Wallet
pwlUpdateWallet logTrace wId wUpdate = do
    walletDB    <- askWalletDB

    cWId        <- migrate wId
    cWMeta      <- migrate wUpdate

    -- Update the data
    setWalletMeta walletDB cWId cWMeta

    -- Get wallet or throw if missing.
    maybeThrow (WalletNotFound wId) =<< pwlGetWallet logTrace wId

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
    => TraceNamed m
    -> WalletId
    -> NewAccount
    -> m (Either CreateAccountError Account)
pwlCreateAccount logTrace wId newAcc@NewAccount{..} = do

    let spendingPassword = fromMaybe mempty . fmap coerce $ naccSpendingPassword

    accInit     <- migrate (wId, newAcc)
    cAccount    <- V0.newAccount logTrace RandomSeed spendingPassword accInit

    Right <$> migrate cAccount

pwlGetAccounts
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => TraceNamed m
    -> WalletId
    -> m (Either GetAccountsError (IxSet Account))
pwlGetAccounts logTrace wId = do
    cWId        <- migrate wId
    cAccounts   <- V0.getAccounts logTrace $ Just cWId
    Right . IxSet.fromList <$> migrate cAccounts

pwlGetAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => TraceNamed m
    -> WalletId
    -> AccountIndex
    -> m (Either GetAccountError Account)
pwlGetAccount logTrace wId aId = do
    accId       <- migrate (wId, aId)
    account     <- V0.getAccount logTrace accId
    Right <$> migrate account

pwlUpdateAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => TraceNamed m
    -> WalletId
    -> AccountIndex
    -> AccountUpdate
    -> m (Either UpdateAccountError Account)
pwlUpdateAccount logTrace wId accIdx accUpdate = do
    newAccId    <- migrate (wId, accIdx)
    accMeta     <- migrate accUpdate
    cAccount    <- V0.updateAccount logTrace newAccId accMeta
    Right <$> migrate cAccount

pwlDeleteAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> m (Either DeleteAccountError ())
pwlDeleteAccount wId accIdx = do
    accId <- migrate (wId, accIdx)
    _ <- catchAll (const True <$> V0.deleteAccount accId) (const . pure $ False)
    return $ Right ()

------------------------------------------------------------
-- Address
------------------------------------------------------------

pwlCreateAddress :: NewAddress -> m (Either CreateAddressError Address)
pwlCreateAddress = error "Not implemented!"

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
