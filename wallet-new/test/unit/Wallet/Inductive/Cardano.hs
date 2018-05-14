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
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Pos.Core (Coin, HasConfiguration, unsafeIntegerToCoin)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Txp (Utxo, formatUtxo)
import           Pos.Util.Chrono

import           Util
import           Util.Validated
import           UTxO.Context (Addr)
import           UTxO.DSL (Hash)
import qualified UTxO.DSL as DSL
import           UTxO.Interpreter
import           UTxO.Translate
import           Wallet.Abstract
import           Wallet.Inductive

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
      walletBootT       :: InductiveCtxt h -> Utxo -> m Kernel.WalletId

      -- | Apply a block
    , walletApplyBlockT :: InductiveCtxt h -> Kernel.WalletId -> RawResolvedBlock -> m ()

      -- | Insert new pending transaction
    , walletNewPendingT :: InductiveCtxt h -> Kernel.WalletId -> RawResolvedTx -> m ()

      -- | Rollback
      --
      -- TODO: Do we want to call 'switch' here? If so, we need some of the logic
      -- from the wallet worker thread to collapse multiple rollbacks and
      -- apply blocks into a single call to switch
    , walletRollbackT   :: InductiveCtxt h -> Kernel.WalletId -> m ()
    }

-- | The context in which a function of 'EventCallbacks' gets called
data InductiveCtxt h = InductiveCtxt {
      -- | The events that led to this point
      inductiveCtxtEvents :: OldestFirst [] (WalletEvent h Addr)

      -- | The 'IntCtxt' suitable for translation derived values
      -- (such as UTxOs)
    , inductiveCtxtInt    :: IntCtxt h

      -- | The pure wallet value at this point
    , inductiveCtxtWallet :: Wallet h Addr
    }

-- | Interpreter for inductive wallets using the translated Cardano types
interpretT :: forall h e m. (Monad m, Hash h Addr)
           => (DSL.Transaction h Addr -> Wallet h Addr)
           -> EventCallbacks h (TranslateT e m)
           -> Inductive h Addr
           -> TranslateT (Either IntException e) m (Wallet h Addr, IntCtxt h)
interpretT mkWallet EventCallbacks{..} Inductive{..} =
    goBoot inductiveBoot
  where
    goBoot :: DSL.Transaction h Addr
           -> TranslateT (Either IntException e) m (Wallet h Addr, IntCtxt h)
    goBoot boot = do
        let w' = mkWallet boot
        initCtxt <- mapTranslateErrors Left $ initIntCtxt boot
        runIntT initCtxt $ do
          let history = NewestFirst []
          utxo' <- int (utxo w') -- translating UTxO does not change the state
          let ctxt = InductiveCtxt (toOldestFirst history) initCtxt w'
          wid   <- liftTranslate $ walletBootT ctxt utxo'
          goEvents wid history w' (getOldestFirst inductiveEvents)

    goEvents :: Kernel.WalletId
             -> NewestFirst [] (WalletEvent h Addr)
             -> Wallet h Addr
             -> [WalletEvent h Addr]
             -> IntT h e m (Wallet h Addr)
    goEvents wid = go
      where
        go :: NewestFirst [] (WalletEvent h Addr)
           -> Wallet h Addr
           -> [WalletEvent h Addr]
           -> IntT h e m (Wallet h Addr)
        go _ w [] =
            return w
        go history w (ApplyBlock b:es) = do
            let history' = liftNewestFirst (ApplyBlock b :) history
                w'       = applyBlock w b
            b' <- int b
            ic <- get
            let ctxt = InductiveCtxt (toOldestFirst history') ic w'
            liftTranslate $ walletApplyBlockT ctxt wid b'
            go history' w' es
        go history w (NewPending t:es) = do
            let history'  = liftNewestFirst (NewPending t :) history
                (Just w') = newPending w t
            t' <- int t
            ic <- get
            let ctxt = InductiveCtxt (toOldestFirst history') ic w'
            liftTranslate $ walletNewPendingT ctxt wid t'
            go history' w' es
        go history w (Rollback:es) = do
            let history' = liftNewestFirst (Rollback :) history
                w'       = rollback w
            ic <- get
            let ctxt = InductiveCtxt (toOldestFirst history') ic w'
            liftTranslate $ walletRollbackT ctxt wid
            go history' w' es

{-------------------------------------------------------------------------------
  Equivalence check between the real implementation and (a) pure wallet
-------------------------------------------------------------------------------}

equivalentT :: forall h m. (HasConfiguration, Hash h Addr, MonadIO m)
            => Kernel.ActiveWallet
            -> EncryptedSecretKey
            -> (DSL.Transaction h Addr -> Wallet h Addr)
            -> Inductive h Addr
            -> TranslateT IntException m (Validated (EquivalenceViolation h) ())
equivalentT activeWallet esk = \mkWallet w ->
      fmap (void . validatedFromEither)
    $ catchSomeTranslateErrors
    $ interpretT mkWallet EventCallbacks{..} w
  where
    passiveWallet = Kernel.walletPassive activeWallet

    walletBootT :: InductiveCtxt h
                -> Utxo
                -> TranslateT (EquivalenceViolation h) m Kernel.WalletId
    walletBootT ctxt utxo = do
        wid <- liftIO $ Kernel.newWalletHdRnd passiveWallet esk utxo
        checkWalletState ctxt wid
        return wid

    walletApplyBlockT :: InductiveCtxt h
                      -> Kernel.WalletId
                      -> RawResolvedBlock
                      -> TranslateT (EquivalenceViolation h) m ()
    walletApplyBlockT ctxt wid block = do
        liftIO $ Kernel.applyBlock passiveWallet (fromRawResolvedBlock block)
        checkWalletState ctxt wid

    walletNewPendingT :: InductiveCtxt h
                      -> Kernel.WalletId
                      -> RawResolvedTx
                      -> TranslateT (EquivalenceViolation h) m ()
    walletNewPendingT ctxt wid tx = do
        _isValid <- liftIO $ Kernel.newPending activeWallet wid (rawResolvedTx tx)
        checkWalletState ctxt wid

    walletRollbackT :: InductiveCtxt h
                    -> Kernel.WalletId
                    -> TranslateT (EquivalenceViolation h) m ()
    walletRollbackT _ _ = error "walletRollbackT: TODO"

    checkWalletState :: InductiveCtxt h
                     -> Kernel.WalletId
                     -> TranslateT (EquivalenceViolation h) m ()
    checkWalletState ctxt@InductiveCtxt{..} wid = do
        cmp "utxo" utxo (`Kernel.getWalletUtxo` wid)
        cmp "totalBalance" totalBalance getWalletTotalBalance
        -- TODO: check other properties
      where
        getWalletTotalBalance :: Kernel.PassiveWallet -> IO Coin
        getWalletTotalBalance pw = unsafeIntegerToCoin <$> Kernel.totalBalance pw wid

        cmp :: ( Interpret h a
               , Eq (Interpreted a)
               , Buildable a
               , Buildable (Interpreted a)
               )
            => Text
            -> (Wallet h Addr -> a)
            -> (Kernel.PassiveWallet -> IO (Interpreted a))
            -> TranslateT (EquivalenceViolation h) m ()
        cmp fld f g = do
          let dsl = f inductiveCtxtWallet
          translated <- toCardano ctxt fld dsl
          kernel     <- liftIO $ g passiveWallet

          unless (translated == kernel) $
            throwError EquivalenceViolation {
                equivalenceViolationName     = fld
              , equivalenceViolationEvents   = inductiveCtxtEvents
              , equivalenceViolationEvidence = NotEquivalent {
                    notEquivalentDsl        = dsl
                  , notEquivalentTranslated = translated
                  , notEquivalentKernel     = kernel
                  }
              }

    toCardano :: Interpret h a
              => InductiveCtxt h
              -> Text
              -> a -> TranslateT (EquivalenceViolation h) m (Interpreted a)
    toCardano InductiveCtxt{..} fld a = do
        ma' <- catchTranslateErrors $ runIntT' inductiveCtxtInt $ int a
        case ma' of
          Left err -> throwError $ EquivalenceNotChecked {
              equivalenceNotCheckedName   = fld
            , equivalenceNotCheckedReason = err
            , equivalenceNotCheckedEvents = inductiveCtxtEvents
            }
          Right (a', _ic') ->
            return a'

data EquivalenceViolation h =
    -- | Cardano wallet and pure wallet are not equivalent
    EquivalenceViolation {
        -- | The property we were checking
        equivalenceViolationName     :: Text

        -- | Evidence (what was not the same?)
      , equivalenceViolationEvidence :: EquivalenceViolationEvidence

        -- | The events that led to the error
      , equivalenceViolationEvents   :: OldestFirst [] (WalletEvent h Addr)
      }

    -- | We got an unexpected interpretation exception
    --
    -- This indicates a bug in the tesing infrastructure.
  | EquivalenceNotChecked {
        -- | The property we were checking
        equivalenceNotCheckedName   :: Text

        -- | Why did we not check the equivalence
      , equivalenceNotCheckedReason :: IntException

        -- | The events that led to the error
      , equivalenceNotCheckedEvents :: OldestFirst [] (WalletEvent h Addr)
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

instance Hash h Addr => Buildable (EquivalenceViolation h) where
  build EquivalenceViolation{..} = bprint
    ( "EquivalenceViolation "
    % "{ name:      " % build
    % ", evidence:  " % build
    % ", events:    " % build
    % "}"
    )
    equivalenceViolationName
    equivalenceViolationEvidence
    equivalenceViolationEvents
  build (EquivalenceNotChecked{..}) = bprint
    ( "EquivalenceNotChecked "
    % "{ name:      " % build
    % ", reason:    " % build
    % ", events:    " % build
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
