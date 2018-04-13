{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Wallet.Abstract.Cardano (
    -- * Cardano interpreter for the inductive wallet
    InductiveT(..)
  , interpretT
    -- * Equivalence check
  , EquivalenceViolation(..)
  , EquivalenceViolationEvidence(..)
  , equivalentT
  ) where

import           Universum
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Pos.Txp (Utxo, formatUtxo)
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.Types

import           Util.Validated
import           UTxO.Context (Addr)
import           UTxO.DSL (Hash)
import qualified UTxO.DSL as DSL
import           UTxO.Interpreter
import           UTxO.Translate
import           Wallet.Abstract

{-------------------------------------------------------------------------------
  Interpreter for the wallet using the translated Cardano types
-------------------------------------------------------------------------------}

-- | Callbacks used in 'interpretT'
--
-- We do not run the callback in the 'IntT' monad so that we maintain
-- control over the interpretation context.
data InductiveT h m = InductiveT {
      -- | Initialize the wallet
      --
      -- The callback is given the translated UTxO of the bootstrap
      -- transaction (we cannot give it the translated transaction because
      -- we cannot translate the bootstrap transaction).
      walletBootT :: InductiveCtxt h -> Utxo -> m ()

      -- | Apply a block
    , walletApplyBlockT :: InductiveCtxt h -> RawResolvedBlock -> m ()

      -- | Insert new pending transaction
    , walletNewPendingT :: InductiveCtxt h -> RawResolvedTx -> m ()
    }

-- | The context in which a function of 'InductiveT' gets called
data InductiveCtxt h = InductiveCtxt {
      -- | The 'Inductive' value that led to this point
      inductiveCtxtInd :: Inductive h Addr

      -- | The 'IntCtxt' suitable for translation derived values
      -- (such as UTxOs)
    , inductiveCtxtInt :: IntCtxt h

      -- | The pure wallet value at this point
    , inductiveCtxtWallet :: Wallet h Addr
    }

-- | Interpreter for inductive wallets using the translated Cardano types
interpretT :: forall h e m. (Monad m, Hash h Addr)
           => (DSL.Transaction h Addr -> Wallet h Addr)
           -> InductiveT h (TranslateT e m)
           -> Inductive h Addr
           -> TranslateT (Either IntException e) m (Wallet h Addr, IntCtxt h)
interpretT mkWallet InductiveT{..} =
    -- This is ugly, but we only discover the bootstrap transaction once we
    -- descend down the 'Inductive' wallet. We will 'put' the right context
    -- before the first call to 'int'.
    runIntT (error "initialized later") . go
  where
    go :: Inductive h Addr -> IntT h e m (Wallet h Addr)
    go ind@(WalletBoot t) = do
        let w' = mkWallet t
        ic <- liftTranslateInt (initIntCtxt t)
        put ic
        utxo' <- int (utxo w') -- translating UTxO does not change the state
        liftTranslate $ walletBootT (InductiveCtxt ind ic w') utxo'
        return w'
    go ind@(ApplyBlock w b) = do
        w' <- go w
        b' <- int b
        ic <- get
        liftTranslate $ walletApplyBlockT (InductiveCtxt ind ic w') b'
        return w'
    go ind@(NewPending w t) = do
        w' <- go w
        t' <- int t
        ic <- get
        liftTranslate $ walletNewPendingT (InductiveCtxt ind ic w') t'
        return w'

{-------------------------------------------------------------------------------
  Equivalence check between the real implementation and (a) pure wallet
-------------------------------------------------------------------------------}

equivalentT :: forall h m. (Hash h Addr, MonadIO m)
            => Kernel.ActiveWallet
            -> (DSL.Transaction h Addr -> Wallet h Addr)
            -> Inductive h Addr
            -> TranslateT IntException m
                 (Validated (EquivalenceViolation h) (Wallet h Addr, IntCtxt h))
equivalentT activeWallet = \mkWallet w ->
      fmap validatedFromEither
    $ catchSomeTranslateErrors
    $ interpretT mkWallet InductiveT{..} w
  where
    passiveWallet = Kernel.walletPassive activeWallet

    walletBootT :: InductiveCtxt h
                -> Utxo
                -> TranslateT (EquivalenceViolation h) m ()
    walletBootT ctxt utxo = do
        liftIO $ Kernel.init passiveWallet utxo
        checkWalletState ctxt

    walletApplyBlockT :: InductiveCtxt h
                      -> RawResolvedBlock
                      -> TranslateT (EquivalenceViolation h) m ()
    walletApplyBlockT ctxt block = do
        liftIO $ Kernel.applyBlock passiveWallet (fromRawResolvedBlock block)
        checkWalletState ctxt

    walletNewPendingT :: InductiveCtxt h
                      -> RawResolvedTx
                      -> TranslateT (EquivalenceViolation h) m ()
    walletNewPendingT ctxt tx = do
        liftIO $ Kernel.newPending activeWallet (fst tx)
        checkWalletState ctxt

    checkWalletState :: InductiveCtxt h
                     -> TranslateT (EquivalenceViolation h) m ()
    checkWalletState ctxt@InductiveCtxt{..} = do
        cmp "utxo" utxo Kernel.utxo
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
                equivalenceViolationName      = fld
              , equivalenceViolationInductive = inductiveCtxtInd
              , equivalenceViolationEvidence  = NotEquivalent {
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
              equivalenceNotCheckedName      = fld
            , equivalenceNotCheckedReason    = err
            , equivalenceNotCheckedInductive = inductiveCtxtInd
            }
          Right (a', _ic') ->
            return a'

data EquivalenceViolation h =
    -- | Cardano wallet and pure wallet are not equivalent
    EquivalenceViolation {
        -- | The property we were checking
        equivalenceViolationName :: Text

        -- | Evidence (what was not the same?)
      , equivalenceViolationEvidence :: EquivalenceViolationEvidence

        -- | The 'Inductive' value at the point of the error
      , equivalenceViolationInductive :: Inductive h Addr
      }

    -- | We got an unexpected interpretation exception
    --
    -- This indicates a bug in the tesing infrastructure.
  | EquivalenceNotChecked {
        -- | The property we were checking
        equivalenceNotCheckedName :: Text

        -- | Why did we not check the equivalence
      , equivalenceNotCheckedReason :: IntException

        -- | The 'Inductive' value at the point of the error
      , equivalenceNotCheckedInductive :: Inductive h Addr
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
    % ", inductive: " % build
    % "}"
    )
    equivalenceViolationName
    equivalenceViolationEvidence
    equivalenceViolationInductive
  build (EquivalenceNotChecked{..}) = bprint
    ( "EquivalenceNotChecked "
    % "{ name:      " % build
    % ", reason:    " % build
    % ", inductive: " % build
    % "}"
    )
    equivalenceNotCheckedName
    equivalenceNotCheckedReason
    equivalenceNotCheckedInductive

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
