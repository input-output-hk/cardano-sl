{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
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

import           Universum

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.Types
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Pos.Chain.Txp (Utxo, formatUtxo)
import           Pos.Core (HasConfiguration)
import           Pos.Core.Chrono
import           Pos.Crypto (EncryptedSecretKey)

import           Cardano.Wallet.Kernel.ChainState (dummyChainBrief)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import qualified Cardano.Wallet.Kernel.Internal as Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.PrefilterTx (prefilterUtxo)

import           Util.Buildable
import           Util.Validated
import           UTxO.Context (Addr)
import           UTxO.DSL (Hash)
import qualified UTxO.DSL as DSL
import           UTxO.Interpreter
import           UTxO.Translate
import           Wallet.Abstract
import           Wallet.Inductive
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
      walletBootT       :: HasConfiguration => InductiveCtxt h -> Utxo -> m HD.HdAccountId

      -- | Apply a block
    , walletApplyBlockT :: HasConfiguration => InductiveCtxt h -> HD.HdAccountId -> RawResolvedBlock -> m ()

      -- | Insert new pending transaction
    , walletNewPendingT :: InductiveCtxt h -> HD.HdAccountId -> RawResolvedTx -> m ()

      -- | Rollback
      --
      -- TODO: Do we want to call 'switch' here? If so, we need some of the logic
      -- from the wallet worker thread to collapse multiple rollbacks and
      -- apply blocks into a single call to switch
    , walletRollbackT   :: InductiveCtxt h -> HD.HdAccountId -> m ()
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
           => (History -> IntException -> e)
           -> (DSL.Transaction h Addr -> Wallet h Addr)
           -> EventCallbacks h (TranslateT e m)
           -> Inductive h Addr
           -> TranslateT e m (Wallet h Addr, IntCtxt h)
interpretT injIntEx mkWallet EventCallbacks{..} Inductive{..} =
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
          (getOldestFirst inductiveEvents)

    goEvents :: HD.HdAccountId
             -> IntCtxt h
             -> History
             -> Wallet h Addr
             -> [WalletEvent h Addr]
             -> TranslateT e m (Wallet h Addr, IntCtxt h)
    goEvents accountId = go
      where
        go :: IntCtxt h
           -> History
           -> Wallet h Addr
           -> [WalletEvent h Addr]
           -> TranslateT e m (Wallet h Addr, IntCtxt h)
        go ctxts _ w [] =
            return (w, ctxts)
        go ic hist w (ApplyBlock b:es) = do
            let w'    = applyBlock w b
                hist' = kernelEvent hist (ApplyBlock b) w'
            ((b', _mEBB), ic') <- int' hist' ic b
            let hist''  = kernelInt hist' ic'
                indCtxt = InductiveCtxt hist'' ic' w'
            -- TODO: Currently we don't pass the EBB to the wallet. Should we?
            withConfig $ walletApplyBlockT indCtxt accountId b'
            go ic' hist'' w' es
        go ic hist w (NewPending t:es) = do
            let Just w' = newPending w t
                hist'   = kernelEvent hist (NewPending t) w'
            (t', ic') <- int' hist' ic t
            let hist''  = kernelInt hist' ic'
                indCtxt = InductiveCtxt hist'' ic' w'
            withConfig $ walletNewPendingT indCtxt accountId t'
            go ic' hist'' w' es
        go ic hist w (Rollback:es) = do
            let w'      = rollback w
                hist'   = kernelEvent hist Rollback w'
            ((), ic') <- int' hist' ic IntRollback
            let hist''  = kernelRollback hist' ic'
                indCtxt = InductiveCtxt hist'' ic' w'
            withConfig $ walletRollbackT indCtxt accountId
            go ic' hist'' w' es

    int' :: Interpret h a
         => History
         -> IntCtxt h
         -> a
         -> TranslateT e m (Interpreted a, IntCtxt h)
    int' hist ic = mapTranslateErrors (injIntEx hist) . runIntT' ic . int

{-------------------------------------------------------------------------------
  Equivalence check between the real implementation and (a) pure wallet
-------------------------------------------------------------------------------}

equivalentT :: forall h e m. (Hash h Addr, MonadIO m, MonadFail m)
            => Kernel.ActiveWallet
            -> EncryptedSecretKey
            -> (DSL.Transaction h Addr -> Wallet h Addr)
            -> Inductive h Addr
            -> TranslateT e m (Validated EquivalenceViolation ())
equivalentT activeWallet esk = \mkWallet w ->
    fmap (void . validatedFromEither)
      $ catchTranslateErrors
      $ interpretT notChecked mkWallet EventCallbacks{..} w
  where
    passiveWallet = Kernel.walletPassive activeWallet

    notChecked :: History -> IntException -> EquivalenceViolation
    notChecked history ex = EquivalenceNotChecked {
          equivalenceNotCheckedName   = "<error during interpretation>"
        , equivalenceNotCheckedReason = ex
        , equivalenceNotCheckedEvents = history
        }

    walletBootT :: InductiveCtxt h
                -> Utxo
                -> TranslateT EquivalenceViolation m HD.HdAccountId
    walletBootT ctxt utxo = do
        res <- liftIO $ Kernel.createWalletHdRnd passiveWallet
                                                 False
                                                 walletName
                                                 assuranceLevel
                                                 esk
                                                 utxo
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
                case length accountIds' of
                    1 -> List.head accountIds'
                    0 -> error "ERROR: no accountIds generated for the given Utxo"
                    _ -> error "ERROR: multiple AccountIds, only one expected"

    walletApplyBlockT :: InductiveCtxt h
                      -> HD.HdAccountId
                      -> RawResolvedBlock
                      -> TranslateT EquivalenceViolation m ()
    walletApplyBlockT ctxt accountId block = do
        liftIO $ Kernel.applyBlock passiveWallet (fromRawResolvedBlock dummyChainBrief block)
        checkWalletState ctxt accountId

    walletNewPendingT :: InductiveCtxt h
                      -> HD.HdAccountId
                      -> RawResolvedTx
                      -> TranslateT EquivalenceViolation m ()
    walletNewPendingT ctxt accountId tx = do
        _ <- liftIO $ Kernel.newPending activeWallet accountId (rawResolvedTx tx)
        checkWalletState ctxt accountId

    walletRollbackT :: InductiveCtxt h
                    -> HD.HdAccountId
                    -> TranslateT EquivalenceViolation m ()
    walletRollbackT ctxt accountId = do
        -- We assume the wallet is not in restoration mode
        Right () <- liftIO $ Kernel.observableRollbackUseInTestsOnly passiveWallet
        checkWalletState ctxt accountId

    checkWalletState :: InductiveCtxt h
                     -> HD.HdAccountId
                     -> TranslateT EquivalenceViolation m ()
    checkWalletState ctxt@InductiveCtxt{..} accountId = do
        snapshot <- liftIO (Kernel.getWalletSnapshot passiveWallet)
        cmp "utxo"          utxo         (snapshot `Kernel.accountUtxo` accountId)
        cmp "totalBalance"  totalBalance (snapshot `Kernel.accountTotalBalance` accountId)
        -- TODO: check other properties
      where
        cmp :: ( Interpret h a
               , Eq (Interpreted a)
               , Buildable a
               , Buildable (Interpreted a)
               )
            => Text
            -> (Wallet h Addr -> a)
            -> Interpreted a
            -> TranslateT EquivalenceViolation m ()
        cmp fld f kernel = do
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

    toCardano :: Interpret h a
              => InductiveCtxt h
              -> Text
              -> a -> TranslateT EquivalenceViolation m (Interpreted a)
    toCardano InductiveCtxt{..} fld a = do
        ma' <- catchTranslateErrors $ runIntT' inductiveCtxtInt $ int a
        case ma' of
          Left err -> throwError
              $ EquivalenceNotChecked fld err inductiveCtxtEvents
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
      , equivalenceNotCheckedReason :: IntException

        -- | The events that led to the error
      , equivalenceNotCheckedEvents :: History
      }

data EquivalenceViolationEvidence =
    forall a. (Buildable a, Buildable (Interpreted a)) => NotEquivalent {
        notEquivalentDsl        :: a
      , notEquivalentTranslated :: Interpreted a
      , notEquivalentKernel     :: Interpreted a
      }

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
