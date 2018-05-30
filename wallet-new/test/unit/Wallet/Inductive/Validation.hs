module Wallet.Inductive.Validation (
    ValidatedInductive(..)
  , InductiveValidationError(..)
  , inductiveIsValid
  ) where

import           Universum

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Pos.Util.Chrono

import           Util
import           Util.Validated
import           UTxO.DSL
import           Wallet.Inductive

{-------------------------------------------------------------------------------
  Successful validation result
-------------------------------------------------------------------------------}

-- | Result of validating an inductive wallet
data ValidatedInductive h a = ValidatedInductive {
      -- | Bootstrap transaction used
      viBoot   :: Transaction h a

      -- | Final ledger (including bootstrap)
    , viLedger :: Ledger h a

      -- | Validated events
    , viEvents :: NewestFirst [] (WalletEvent h a)

      -- | Final chain (split into blocks, not including bootstrap)
    , viChain  :: NewestFirst [] (Block h a)

      -- | UTxO after each block
    , viUtxos  :: NewestFirst NonEmpty (Utxo h a)
    }

{-------------------------------------------------------------------------------
  Validation errors
-------------------------------------------------------------------------------}

data InductiveValidationError h a =
    -- | Bootstrap transaction is invalid
    InductiveInvalidBoot {
        -- | The bootstrap transaction
        inductiveInvalidBoot  :: Transaction h a

        -- | The error message
      , inductiveInvalidError :: Text
      }

    -- | Invalid transaction in the given block
  | InductiveInvalidApplyBlock {
        -- | The events leading up to the error
        inductiveInvalidEvents      :: OldestFirst [] (WalletEvent h a)

        -- | The transactions in the block we successfully validated
      , inductiveInvalidBlockPrefix :: OldestFirst [] (Transaction h a)

        -- | The transaction that was invalid
      , inductiveInvalidTransaction :: Transaction h a

        -- | The error message
      , inductiveInvalidError       :: Text
      }

    -- | A 'NewPending' call was invalid because the input was already spent
  | InductiveInvalidNewPendingAlreadySpent {
        -- | The events leading up to the error
        inductiveInvalidEvents      :: OldestFirst [] (WalletEvent h a)

        -- | The transaction that was invalid
      , inductiveInvalidTransaction :: Transaction h a

        -- | The specific input that was not valid
      , inductiveInvalidInput       :: Input h a
      }

    -- | A 'NewPending' call was invalid because the input was not @ours@
  | InductiveInvalidNewPendingNotOurs {
        -- | The events leading up to the error
        inductiveInvalidEvents      :: OldestFirst [] (WalletEvent h a)

        -- | The transaction that was invalid
      , inductiveInvalidTransaction :: Transaction h a

        -- | The specific input that was not valid
      , inductiveInvalidInput       :: Input h a

        -- | The address this input belonged to
      , inductiveInvalidAddress     :: a
      }

{-------------------------------------------------------------------------------
  Validation proper
-------------------------------------------------------------------------------}

-- | Lift ledger validity to 'Inductive'
inductiveIsValid :: forall h a. (Hash h a, Buildable a, Ord a)
                 => Inductive h a
                 -> Validated (InductiveValidationError h a) (ValidatedInductive h a)
inductiveIsValid Inductive{..} = do
    goBoot inductiveBoot
  where
    goBoot :: Transaction h a
           -> Validated (InductiveValidationError h a) (ValidatedInductive h a)
    goBoot boot = do
        let ledger = ledgerEmpty
        validatedMapErrors (InductiveInvalidBoot boot) $
          trIsAcceptable boot ledger
        goEvents (getOldestFirst inductiveEvents) ValidatedInductive {
            viBoot   = boot
          , viLedger = ledgerAdd boot ledger
          , viEvents = NewestFirst []
          , viChain  = NewestFirst []
          , viUtxos  = NewestFirst (trUtxo boot :| [])
          }

    goEvents :: [WalletEvent h a]
             -> ValidatedInductive h a -- accumulator
             -> Validated (InductiveValidationError h a) (ValidatedInductive h a)
    goEvents [] acc =
        return acc
    goEvents (ApplyBlock b:es) ValidatedInductive{..} = do
        ledger' <- goBlock (toOldestFirst viEvents) (OldestFirst []) viLedger b
        goEvents es ValidatedInductive {
            viBoot   = viBoot
          , viLedger = ledger'
          , viEvents = liftNewestFirst (ApplyBlock b :) viEvents
          , viChain  = liftNewestFirst (           b :) viChain
          , viUtxos  = newCheckpoint               b    viUtxos
          }
    goEvents (Rollback:es) ValidatedInductive{..} = do
      let chain' = liftNewestFirst List.tail viChain
      goEvents es ValidatedInductive {
          viBoot   = viBoot
        , viLedger = revChainToLedger chain'
        , viEvents = liftNewestFirst (Rollback :) viEvents
        , viChain  = chain'
        , viUtxos  = prevCheckpoint viUtxos
        }
    goEvents (NewPending t:es) vi@ValidatedInductive{..} = do
        let utxo     = let NewestFirst (u :| _) = viUtxos in u
            inputs   = Set.toList (trIns t)
            resolved = map (`utxoAddressForInput` utxo) inputs
        forM_ (zip inputs resolved) $ \(input, mAddr) ->
          case mAddr of
            Nothing ->
              throwError InductiveInvalidNewPendingAlreadySpent {
                  inductiveInvalidEvents      = toOldestFirst viEvents
                , inductiveInvalidTransaction = t
                , inductiveInvalidInput       = input
                }
            Just addr ->
              unless (addr `Set.member` inductiveOurs) $
                throwError InductiveInvalidNewPendingNotOurs {
                    inductiveInvalidEvents      = toOldestFirst viEvents
                  , inductiveInvalidTransaction = t
                  , inductiveInvalidInput       = input
                  , inductiveInvalidAddress     = addr
                  }
        goEvents es vi

    goBlock :: OldestFirst [] (WalletEvent h a) -- Events leading to this point (for err msgs)
            -> Block h a  -- Prefix of the block already validated (for err msgs)
            -> Ledger h a -- Ledger so far
            -> Block h a  -- Suffix of the block yet to validate
            -> Validated (InductiveValidationError h a) (Ledger h a)
    goBlock events = go
      where
        go _ ledger (OldestFirst []) =
          return ledger
        go (OldestFirst done) ledger (OldestFirst (t:todo)) = do
          validatedMapErrors (InductiveInvalidApplyBlock events (OldestFirst done) t) $
            trIsAcceptable t ledger
          go (OldestFirst (done ++ [t])) (ledgerAdd t ledger) (OldestFirst todo)

    revChainToLedger :: NewestFirst [] (Block h a) -> Ledger h a
    revChainToLedger = Ledger
                     . NewestFirst
                     . (inductiveBoot :)
                     . concatMap toList . toList

    newCheckpoint :: Block h a
                  -> NewestFirst NonEmpty (Utxo h a)
                  -> NewestFirst NonEmpty (Utxo h a)
    newCheckpoint b = liftNewestFirst $ \(u :| us) ->
        utxoApplyBlock b u :| (u:us)

    prevCheckpoint :: NewestFirst NonEmpty (Utxo h a)
                   -> NewestFirst NonEmpty (Utxo h a)
    prevCheckpoint = liftNewestFirst $ \(_u :| (u':us))
        -> u' :| us

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (InductiveValidationError h a) where
  build InductiveInvalidBoot{..} = bprint
    ( "InductiveInvalidBoot"
    % "{ boot:  " % build
    % ", error: " % build
    % "}"
    )
    inductiveInvalidBoot
    inductiveInvalidError
  build InductiveInvalidApplyBlock{..} = bprint
    ( "InductiveInvalidApplyBlock"
    % "{ events:      " % build
    % ", blockPrefix: " % build
    % ", transaction: " % build
    % ", error:       " % build
    % "}")
    inductiveInvalidEvents
    inductiveInvalidBlockPrefix
    inductiveInvalidTransaction
    inductiveInvalidError
  build InductiveInvalidNewPendingAlreadySpent{..} = bprint
    ( "InductiveInvalidNewPendingAlreadySpent"
    % "{ events:      " % build
    % ", transaction: " % build
    % ", input:       " % build
    % "}"
    )
    inductiveInvalidEvents
    inductiveInvalidTransaction
    inductiveInvalidInput
  build InductiveInvalidNewPendingNotOurs{..} = bprint
    ( "InductiveInvalidNewPendingNotOurs"
    % "{ events:      " % build
    % ", transaction: " % build
    % ", input:       " % build
    % ", address:     " % build
    % "}"
    )
    inductiveInvalidEvents
    inductiveInvalidTransaction
    inductiveInvalidInput
    inductiveInvalidAddress
