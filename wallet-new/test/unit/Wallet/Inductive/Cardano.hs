{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}

module Wallet.Inductive.Cardano (
    -- * Cardano interpreter for the inductive wallet
    EventCallbacks(..)
  , interpretT
    -- * Equivalence check
  , EquivalenceViolation(..)
  , EquivalenceViolationEvidence(..)
  , equivalentT
  ) where

import qualified Prelude (show)
import           Universum

import           Cardano.Wallet.Kernel.Types
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import qualified Data.Map.Strict as Map
import           Data.Time.Units (fromMicroseconds)
import           Formatting (bprint, build, formatToString, sformat, (%))
import qualified Formatting.Buildable

import           Pos.Chain.Txp (Utxo, formatUtxo)
import           Pos.Core (Timestamp (..))
import           Pos.Core.Chrono
import           Pos.Crypto (EncryptedSecretKey, emptyPassphrase)

import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import qualified Cardano.Wallet.Kernel.BListener as Kernel
import qualified Cardano.Wallet.Kernel.DB.AcidState as DB
import qualified Cardano.Wallet.Kernel.DB.BlockContext as DB
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import qualified Cardano.Wallet.Kernel.DB.Resolved as DB
import qualified Cardano.Wallet.Kernel.Internal as Internal
import           Cardano.Wallet.Kernel.Invariants as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Pending as Kernel
import           Cardano.Wallet.Kernel.PrefilterTx (prefilterUtxo)
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Transactions (toMeta)

import           Data.Validated
import           Util.Buildable
import           UTxO.Context (Addr)
import           UTxO.DSL (Hash)
import qualified UTxO.DSL as DSL
import           UTxO.ToCardano.Interpreter
import           UTxO.Translate
import           Wallet.Abstract
import           Wallet.Inductive
import           Wallet.Inductive.ExtWalletEvent
import           Wallet.Inductive.History

{-------------------------------------------------------------------------------
  Interpreter for the wallet using the translated Cardano types
-------------------------------------------------------------------------------}

-- | Callbacks used in 'interpretT'
--
-- We do not run the callback in the 'IntT' monad so that we maintain
-- control over the interpretation context.
data EventCallbacks h m = EventCallbacks {
      -- | Initialize the wallet
      --
      -- The callback is given the translated UTxO of the bootstrap
      -- transaction (we cannot give it the translated transaction because
      -- we cannot translate the bootstrap transaction).
      walletBootT :: InductiveCtxt h -> Utxo -> m HD.HdAccountId

      -- | Apply a block
    , walletApplyBlockT :: InductiveCtxt h -> HD.HdAccountId -> RawResolvedBlock -> m ()

      -- | Insert new pending transaction
    , walletNewPendingT :: InductiveCtxt h -> HD.HdAccountId -> RawResolvedTx -> m ()

      -- | Rollback
      --
      -- TODO: Do we want to call 'switch' here? If so, we need some of the logic
      -- from the wallet worker thread to collapse multiple rollbacks and
      -- apply blocks into a single call to switch
    , walletRollbackT :: InductiveCtxt h -> HD.HdAccountId -> m ()

      -- | Switch to fork
    , walletSwitchToForkT :: InductiveCtxt h -> HD.HdAccountId -> Int -> OldestFirst [] RawResolvedBlock -> m ()
    }

-- | The context in which a function of 'EventCallbacks' gets called
data InductiveCtxt h = InductiveCtxt {
      -- | The events that led to this point
      inductiveCtxtEvents :: History

      -- | The 'IntCtxt' suitable for translation derived values
      -- (such as UTxOs)
    , inductiveCtxtInt    :: IntCtxt h

      -- | The pure wallet value at this point
    , inductiveCtxtWallet :: Wallet h Addr
    }

-- | Interpreter for inductive wallets using the translated Cardano types
--
-- Returns the final wallet as well as the "interpretation state checkpoints"
-- (to support any further rollback).
interpretT :: forall h e m. (Monad m, Hash h Addr)
           => UseWalletWorker
           -> (History -> Text -> e) -- ^ Inject exceptions into the errors
           -> (DSL.Transaction h Addr -> Wallet h Addr)
           -> EventCallbacks h (TranslateT e m)
           -> Inductive h Addr
           -> TranslateT e m (Wallet h Addr, IntCtxt h)
interpretT useWW injErr mkWallet EventCallbacks{..} Inductive{..} =
    goBoot inductiveBoot
  where
    goBoot :: DSL.Transaction h Addr
           -> TranslateT e m (Wallet h Addr, IntCtxt h)
    goBoot boot = do
        ic <- initIntCtxt boot
        let w'   = mkWallet boot
            hist = kernelInit w' ic
        (utxo', ic') <- int' hist ic $ utxo w'
        let hist'   = kernelInt hist ic'
            indCtxt = InductiveCtxt hist' ic' w'
        accountId <- withConfig $ walletBootT indCtxt utxo'
        goEvents
          accountId
          ic'
          hist
          w'
          (getOldestFirst (extWalletEvents useWW inductiveEvents))

    goEvents :: HD.HdAccountId
             -> IntCtxt h
             -> History
             -> Wallet h Addr
             -> [ExtWalletEvent h Addr]
             -> TranslateT e m (Wallet h Addr, IntCtxt h)
    goEvents accountId = go
      where
        go :: IntCtxt h
           -> History
           -> Wallet h Addr
           -> [ExtWalletEvent h Addr]
           -> TranslateT e m (Wallet h Addr, IntCtxt h)
        go ctxts _ w [] =
            return (w, ctxts)
        go ic hist w (e@(ExtApplyBlock b):es) = do
            let w'    = applyBlock w b
                hist' = kernelEvent hist e w'
            ((b', _mEBB), ic') <- int' hist' ic b
            let hist''  = kernelInt hist' ic'
                indCtxt = InductiveCtxt hist'' ic' w'
            -- TODO: Currently we don't pass the EBB to the wallet. Should we?
            withConfig $ walletApplyBlockT indCtxt accountId b'
            go ic' hist'' w' es
        go ic hist w (e@(ExtNewPending t):es) = do
            case newPending w t of
              Nothing ->
                throwError . injErr hist $
                  sformat ("Invalid pending " % build) t
              Just w' -> do
                let hist'   = kernelEvent hist e w'
                (t', ic') <- int' hist' ic t
                let hist''  = kernelInt hist' ic'
                    indCtxt = InductiveCtxt hist'' ic' w'
                withConfig $ walletNewPendingT indCtxt accountId t'
                go ic' hist'' w' es
        go ic hist w (e@ExtRollback:es) = do
            let w'      = rollback w
                hist'   = kernelEvent hist e w'
            ((), ic') <- int' hist' ic IntRollback
            let hist''  = kernelRollback hist' ic'
                indCtxt = InductiveCtxt hist'' ic' w'
            withConfig $ walletRollbackT indCtxt accountId
            go ic' hist'' w' es
        go ic hist w (e@(ExtSwitchToFork n bs):es) = do
            let w'      = switchToFork w n bs
                hist'   = kernelEvent hist e w'
                bs0     = OldestFirst . toList . getOldestFirst $ bs
            (bs', ic') <- int' hist' ic (IntSwitchToFork n bs0)
            let hist''  = kernelRollback hist' ic'
                indCtxt = InductiveCtxt hist'' ic' w'
            withConfig $ walletSwitchToForkT indCtxt accountId n bs'
            go ic' hist'' w' es

    int' :: Interpret DSL2Cardano h a
         => History
         -> IntCtxt h
         -> a
         -> TranslateT e m (Interpreted DSL2Cardano a, IntCtxt h)
    int' hist ic =
        mapTranslateErrors (injErr hist . pretty) . runIntT' ic . int @DSL2Cardano

{-------------------------------------------------------------------------------
  Equivalence check between the real implementation and (a) pure wallet
-------------------------------------------------------------------------------}

equivalentT :: forall h e m. (Hash h Addr, MonadIO m, MonadFail m)
            => UseWalletWorker
            -> Internal.ActiveWallet
            -> EncryptedSecretKey
            -> (DSL.Transaction h Addr -> Wallet h Addr)
            -> Inductive h Addr
            -> TranslateT e m (Validated EquivalenceViolation (Wallet h Addr, IntCtxt h))
equivalentT useWW activeWallet esk = \mkWallet w ->
    fmap validatedFromEither
      $ catchTranslateErrors
      $ interpretT useWW notChecked mkWallet EventCallbacks{..} w
  where
    passiveWallet = Internal.walletPassive activeWallet

    notChecked :: History -> Text -> EquivalenceViolation
    notChecked history ex = EquivalenceNotChecked {
          equivalenceNotCheckedName   = "<error during interpretation>"
        , equivalenceNotCheckedReason = ex
        , equivalenceNotCheckedEvents = history
        }

    walletBootT :: InductiveCtxt h
                -> Utxo
                -> TranslateT EquivalenceViolation m HD.HdAccountId
    walletBootT ctxt utxo = do
        let newRootId = HD.eskToHdRootId esk
        let (Just defaultAddress) = Kernel.newHdAddress esk
                                                        emptyPassphrase
                                                        (Kernel.defaultHdAccountId newRootId)
                                                        (Kernel.defaultHdAddressId newRootId)
        res <- liftIO $
          Kernel.createWalletHdRnd
            passiveWallet
            False
            (defaultAddress ^. HD.hdAddressAddress . fromDb)
            walletName
            assuranceLevel
            esk
            (\root defaultAccount defAddress ->
                Left $ DB.CreateHdWallet root
                                         defaultAccount
                                         defAddress
                                         (prefilterUtxo (root ^. HD.hdRootId) esk utxo)
            )
        case res of
             Left e -> createWalletErr (STB e)
             Right hdRoot -> do
                 let keystore = passiveWallet ^. Internal.walletKeystore
                 liftIO $ Keystore.insert (WalletIdHdRnd $ hdRoot ^. HD.hdRootId) esk keystore
                 checkWalletAccountState ctxt accountIds

        where
            walletName       = HD.WalletName "(test wallet)"
            assuranceLevel   = HD.AssuranceLevelNormal

            utxoByAccount = prefilterUtxo rootId esk utxo
            accountIds    = Map.keys utxoByAccount
            rootId        = HD.eskToHdRootId esk

            createWalletErr e =
                error $ "ERROR: could not create the HdWallet due to " <> show e

            checkWalletAccountState ctxt' accountIds' = do
                let accountId' = pickSingletonAccountId accountIds'
                checkWalletState ctxt' accountId'
                return accountId'

            -- Since the DSL Wallet does not model Account, a DSL Wallet is expressed
            -- as a Cardano Wallet with exactly one Account.
            -- Here, we safely extract the AccountId.
            pickSingletonAccountId :: [HD.HdAccountId] -> HD.HdAccountId
            pickSingletonAccountId accountIds' =
                case accountIds' of
                    [accId] -> accId
                    []      -> error "ERROR: no accountIds generated for the given Utxo"
                    _       -> error "ERROR: multiple accountIds generated, only one expected"

    walletApplyBlockT :: InductiveCtxt h
                      -> HD.HdAccountId
                      -> RawResolvedBlock
                      -> TranslateT EquivalenceViolation m ()
    walletApplyBlockT ctxt accountId block = do
        -- We assume the wallet is not behind
        liftIO $ Kernel.applyBlock passiveWallet (fromRawResolvedBlock block)
        checkWalletState ctxt accountId

    walletNewPendingT :: InductiveCtxt h
                      -> HD.HdAccountId
                      -> RawResolvedTx
                      -> TranslateT EquivalenceViolation m ()
    walletNewPendingT ctxt accountId tx = do
        let currentTime = getSomeTimestamp
        let partialMeta = toMeta currentTime accountId tx
        _ <- liftIO $ Kernel.newPending activeWallet accountId (rawResolvedTx tx) partialMeta
        checkWalletState ctxt accountId

    walletRollbackT :: InductiveCtxt h
                    -> HD.HdAccountId
                    -> TranslateT EquivalenceViolation m ()
    walletRollbackT ctxt accountId = do
        -- We assume the wallet is not in restoration mode
        Right () <- liftIO $ Kernel.observableRollbackUseInTestsOnly passiveWallet
        checkWalletState ctxt accountId

    walletSwitchToForkT :: InductiveCtxt h
                        -> HD.HdAccountId
                        -> Int
                        -> OldestFirst [] RawResolvedBlock
                        -> TranslateT EquivalenceViolation m ()
    walletSwitchToForkT ctxt accountId _n bs = do
        -- We assume the wallet is not in restoration mode
        let rbs@(oldest:_) = map fromRawResolvedBlock (toList bs)
            hh = _fromDb <$> (oldest ^. DB.rbContext . DB.bcPrevMain)
        liftIO $ Kernel.switchToFork passiveWallet hh rbs
        checkWalletState ctxt accountId

    checkWalletState :: InductiveCtxt h
                     -> HD.HdAccountId
                     -> TranslateT EquivalenceViolation m ()
    checkWalletState ctxt@InductiveCtxt{..} accountId = do
        snapshot <- liftIO (Kernel.getWalletSnapshot passiveWallet)
        cmp "utxo"          utxo         (Kernel.currentUtxo         snapshot accountId)
        cmp "totalBalance"  totalBalance (Kernel.currentTotalBalance snapshot accountId)
        liftIO $ Kernel.checkInvariantSubmission passiveWallet
        -- TODO: check other properties
      where
        cmp :: ( Interpret DSL2Cardano h a
               , Eq (Interpreted DSL2Cardano a)
               , Buildable a
               , Buildable (Interpreted DSL2Cardano a)
               , Buildable err
               )
            => Text
            -> (Wallet h Addr -> a)
            -> Either err (Interpreted DSL2Cardano a)
            -> TranslateT EquivalenceViolation m ()
        cmp fld _ (Left err) =
          throwError $ EquivalenceNotChecked
            fld
            (pretty err)
            inductiveCtxtEvents
        cmp fld f (Right kernel) = do
          let dsl = f inductiveCtxtWallet
          translated <- toCardano ctxt fld dsl

          unless (translated == kernel) .
            throwError $ EquivalenceViolation
              fld
              NotEquivalent {
                    notEquivalentDsl        = dsl
                  , notEquivalentTranslated = translated
                  , notEquivalentKernel     = kernel
                  }
              inductiveCtxtEvents

    toCardano :: Interpret DSL2Cardano h a
              => InductiveCtxt h
              -> Text
              -> a -> TranslateT EquivalenceViolation m (Interpreted DSL2Cardano a)
    toCardano InductiveCtxt{..} fld a = do
        ma' <- catchTranslateErrors $ runIntT' inductiveCtxtInt $ int @DSL2Cardano a
        case ma' of
          Left err -> throwError
              $ EquivalenceNotChecked fld (pretty err) inductiveCtxtEvents
          Right (a', _ic') -> return a'


data EquivalenceViolation =
    -- | Cardano wallet and pure wallet are not equivalent
    EquivalenceViolation {
        -- | The property we were checking
        equivalenceViolationName     :: Text

        -- | Evidence (what wasn't the same?)
      , equivalenceViolationEvidence :: EquivalenceViolationEvidence

        -- | The events that led to the error
      , equivalenceViolationEvents   :: History
      }

    -- | We got an unexpected interpretation exception
    --
    -- This indicates a bug in the tesing infrastructure.
  | EquivalenceNotChecked {
        -- | The property we were checking (if any)
        equivalenceNotCheckedName   :: Text

        -- | Why did we not check the equivalence
      , equivalenceNotCheckedReason :: Text

        -- | The events that led to the error
      , equivalenceNotCheckedEvents :: History
      }

data EquivalenceViolationEvidence =
    forall a. (Buildable a, Buildable (Interpreted DSL2Cardano a)) => NotEquivalent {
        notEquivalentDsl        :: a
      , notEquivalentTranslated :: Interpreted DSL2Cardano a
      , notEquivalentKernel     :: Interpreted DSL2Cardano a
      }

instance Show EquivalenceViolation where
    show = formatToString build

instance Exception EquivalenceViolation

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getSomeTimestamp :: Timestamp
getSomeTimestamp = Timestamp $ fromMicroseconds 12340000

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable EquivalenceViolation where
  build EquivalenceViolation{..} = bprint
    ( "EquivalenceViolation "
    % "{ name:     " % build
    % ", evidence: " % build
    % ", events:   " % build
    % "}"
    )
    equivalenceViolationName
    equivalenceViolationEvidence
    equivalenceViolationEvents
  build EquivalenceNotChecked{..} = bprint
    ( "EquivalenceNotChecked "
    % "{ name:   " % build
    % ", reason: " % build
    % ", events: " % build
    % "}"
    )
    equivalenceNotCheckedName
    equivalenceNotCheckedReason
    equivalenceNotCheckedEvents

instance Buildable EquivalenceViolationEvidence where
  build NotEquivalent{..} = bprint
    ( "NotEquivalent "
    % "{ notEquivalentDsl:        " % build
    % ", notEquivalentTranslated: " % build
    % ", notEquivalentKernel:     " % build
    % "}"
    )
    notEquivalentDsl
    notEquivalentTranslated
    notEquivalentKernel

{-------------------------------------------------------------------------------
  Orphans (TODO: avoid)
-------------------------------------------------------------------------------}

instance Buildable Utxo where
  build = formatUtxo
