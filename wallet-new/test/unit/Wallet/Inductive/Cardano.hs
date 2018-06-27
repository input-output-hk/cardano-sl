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
import qualified Data.List as List
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Pos.Core (AddressHash, HasConfiguration)
import           Pos.Core.Chrono
import           Pos.Crypto (EncryptedSecretKey, PublicKey)
import           Pos.Txp (Utxo, formatUtxo)

import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD

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
          accountId <- liftTranslate $ walletBootT ctxt utxo'
          goEvents accountId history w' (getOldestFirst inductiveEvents)

    goEvents :: HD.HdAccountId
             -> NewestFirst [] (WalletEvent h Addr)
             -> Wallet h Addr
             -> [WalletEvent h Addr]
             -> IntT h e m (Wallet h Addr)
    goEvents accountId = go
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
            liftTranslate $ walletApplyBlockT ctxt accountId b'
            go history' w' es
        go history w (NewPending t:es) = do
            let history'  = liftNewestFirst (NewPending t :) history
                (Just w') = newPending w t
            t' <- int t
            ic <- get
            let ctxt = InductiveCtxt (toOldestFirst history') ic w'
            liftTranslate $ walletNewPendingT ctxt accountId t'
            go history' w' es
        go history w (Rollback:es) = do
            let history' = liftNewestFirst (Rollback :) history
                w'       = rollback w
            ic <- get
            let ctxt = InductiveCtxt (toOldestFirst history') ic w'
            liftTranslate $ walletRollbackT ctxt accountId
            go history' w' es

{-------------------------------------------------------------------------------
  Equivalence check between the real implementation and (a) pure wallet
-------------------------------------------------------------------------------}

equivalentT :: forall h m. (Hash h Addr, MonadIO m)
            => Kernel.ActiveWallet
            -> (AddressHash PublicKey, EncryptedSecretKey)
            -> (DSL.Transaction h Addr -> Wallet h Addr)
            -> Inductive h Addr
            -> TranslateT IntException m (Validated (EquivalenceViolation h) ())
equivalentT activeWallet (pk,esk) = \mkWallet w ->
      fmap (void . validatedFromEither)
          $ catchSomeTranslateErrors
          $ interpretT mkWallet EventCallbacks{..} w
  where
    passiveWallet = Kernel.walletPassive activeWallet

    walletBootT :: InductiveCtxt h
                -> Utxo
                -> TranslateT (EquivalenceViolation h) m HD.HdAccountId
    walletBootT ctxt utxo = do
        res <- liftIO $ Kernel.createWalletHdRnd passiveWallet walletName
                                                 spendingPassword assuranceLevel
                                                 (pk,esk) utxo

        either createWalletErr (checkWalletAccountState ctxt) res

        where
            walletName       = HD.WalletName "(test wallet)"
            spendingPassword = HD.NoSpendingPassword
            assuranceLevel   = HD.AssuranceLevelNormal

            createWalletErr _ = error "ERROR: could not create the HdWallet"

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
                      -> TranslateT (EquivalenceViolation h) m ()
    walletApplyBlockT ctxt accountId block = do
        liftIO $ Kernel.applyBlock passiveWallet (fromRawResolvedBlock block)
        checkWalletState ctxt accountId

    walletNewPendingT :: InductiveCtxt h
                      -> HD.HdAccountId
                      -> RawResolvedTx
                      -> TranslateT (EquivalenceViolation h) m ()
    walletNewPendingT ctxt accountId tx = do
        _ <- liftIO $ Kernel.newPending activeWallet accountId (rawResolvedTx tx)
        checkWalletState ctxt accountId

    walletRollbackT :: InductiveCtxt h
                    -> HD.HdAccountId
                    -> TranslateT (EquivalenceViolation h) m ()
    walletRollbackT _ _ = error "walletRollbackT: TODO"

    checkWalletState :: InductiveCtxt h
                     -> HD.HdAccountId
                     -> TranslateT (EquivalenceViolation h) m ()
    checkWalletState ctxt@InductiveCtxt{..} accountId = do
        cmp "utxo"          utxo         (`Kernel.accountUtxo` accountId)
        cmp "totalBalance"  totalBalance (`Kernel.accountTotalBalance` accountId)
        -- TODO: check other properties
      where
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
          Left err -> throwError EquivalenceNotChecked {
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
  build EquivalenceNotChecked{..} = bprint
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
