{-# LANGUAGE ExistentialQuantification #-}

module Wallet.Inductive.Invariants (
    -- * Invariants
    Invariant
  , invariant
    -- * Failures
  , InvariantViolation(..)
  , InvariantViolationEvidence(..)
    -- * Specific invariants
  , WalletInv
  , ApplicableInvariants(..)
  , walletInvariants
    -- * Equivalence
  , walletEquivalent
  ) where

import           Universum

import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Pos.Util.Chrono
import           Serokell.Util (listJson)

import           Util
import           Util.Validated
import           UTxO.DSL
import           Wallet.Abstract
import           Wallet.Inductive
import           Wallet.Inductive.Interpreter

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

-- | Wallet invariant
--
-- A wallet invariant is a property that is preserved by the fundamental
-- wallet operations, as defined by the 'IsWallet' type class and the
-- definition of 'Inductive'.
--
-- In order to evaluate the inductive definition we need the empty wallet
-- to be passed as a starting point.
type Invariant h a = Inductive h a -> Validated (InvariantViolation h a) ()

-- | Lift a property of flat wallet values to an invariant over the wallet ops
invariant :: forall h a.
             Text                             -- ^ Name of the invariant
          -> (Transaction h a -> Wallet h a)  -- ^ Construct empty wallet
          -> (Wallet h a -> Maybe InvariantViolationEvidence) -- ^ Property
          -> Invariant h a
invariant name e p = void . interpret notChecked ((:[]) . e) p'
  where
    notChecked :: OldestFirst [] (WalletEvent h a)
               -> InvalidInput h a
               -> InvariantViolation h a
    notChecked history reason = InvariantNotChecked {
          invariantNotCheckedName   = name
        , invariantNotCheckedReason = reason
        , invariantNotCheckedEvents = history
        }

    violation :: OldestFirst [] (WalletEvent h a)
              -> InvariantViolationEvidence
              -> InvariantViolation h a
    violation history ev = InvariantViolation {
          invariantViolationName     = name
        , invariantViolationEvidence = ev
        , invariantViolationEvents   = history
        }

    p' :: OldestFirst [] (WalletEvent h a)
       -> [Wallet h a]
       -> Validated (InvariantViolation h a) ()
    p' history [w] = case p w of
                       Nothing -> return ()
                       Just ev -> throwError (violation history ev)
    p' _ _ = error "impossible"

-- | Invariant violation
data InvariantViolation h a =
    -- | Invariance violation
    InvariantViolation {
        -- | Name of the invariant
        invariantViolationName     :: Text

        -- | Evidence that the invariant was violated
      , invariantViolationEvidence :: InvariantViolationEvidence

        -- | The evens that led to the error
      , invariantViolationEvents   :: OldestFirst [] (WalletEvent h a)
      }

    -- | The invariant was not checked because the input was invalid
  | InvariantNotChecked {
        -- | Name of the invariant
        invariantNotCheckedName   :: Text

        -- | Why did we not check the invariant
      , invariantNotCheckedReason :: InvalidInput h a

        -- | The evens that led to the error
      , invariantNotCheckedEvents :: OldestFirst [] (WalletEvent h a)
      }

{-------------------------------------------------------------------------------
  Evidence that an invariant was violated

  Rather than just whether or not that the invariant is maintained, we try
  to produce an informative error message when the invariant is /not/
  maintained so that we can debug what's going on.
-------------------------------------------------------------------------------}

-- | Evidence that the invariance was violated
data InvariantViolationEvidence =
    forall a. Buildable a =>
      NotEqual (Text, a) (Text, a)
  | forall a. (Buildable a, Ord a) =>
      NotSubsetOf (Text, Set a) (Text, Set a)
  | forall a. (Buildable a) =>
      NotAllSatisfy (Text, a -> Bool) (Text, [a])
  | forall a. (Buildable a, Ord a) =>
      NotDisjoint (Text, Set a) (Text, Set a)

checkEqual :: (Buildable a, Eq a)
           => (Text, a) -> (Text, a) -> Maybe InvariantViolationEvidence
checkEqual (labelX, x) (labelY, y) =
    if x == y
      then Nothing
      else Just $ NotEqual (labelX, x) (labelY, y)

checkSubsetOf :: (Buildable a, Ord a)
              => (Text, Set a) -> (Text, Set a) -> Maybe InvariantViolationEvidence
checkSubsetOf (labelXs, xs) (labelYs, ys) =
    if xs `Set.isSubsetOf` ys
      then Nothing
      else Just $ NotSubsetOf (labelXs, xs) (labelYs, ys)

checkAllSatisfy :: Buildable a
                => (Text, a -> Bool) -> (Text, [a]) -> Maybe InvariantViolationEvidence
checkAllSatisfy (labelP, p) (labelXs, xs) =
    if all p xs
      then Nothing
      else Just $ NotAllSatisfy (labelP, p) (labelXs, xs)

checkDisjoint :: (Buildable a, Ord a)
              => (Text, Set a) -> (Text, Set a) -> Maybe InvariantViolationEvidence
checkDisjoint (labelXs, xs) (labelYs, ys) =
    if disjoint xs ys
      then Nothing
      else Just $ NotDisjoint (labelXs, xs) (labelYs, ys)

{-------------------------------------------------------------------------------
  Specific invariants
-------------------------------------------------------------------------------}

-- | Wallet invariant, parameterized by a function to construct the wallet
type WalletInv h a = (Hash h a, Buildable a, Eq a)
                  => Text -> (Transaction h a -> Wallet h a) -> Invariant h a

-- | Which invariants are applicable to this wallet?
data ApplicableInvariants =
    -- | There are no rollbacks in the system
    NoRollback

    -- | We have rollbacks, but we are working in the basic model
  | BasicRollback

    -- | We are working in the full model (including support for expected UTxO)
  | FullRollback

walletInvariants :: ApplicableInvariants -> WalletInv h a
walletInvariants applicableInvariants l e w = do
    -- Invariants applicable in all models
    sequence_ [
        utxoIsOurs             l e w
      , changeNotAvailable     l e w
      , changeNotInUtxo        l e w
      , changeAvailable        l e w
      , balanceChangeAvailable l e w
      ]

    case applicableInvariants of
      -- Invariants that only hold when there are no rollbacks
      NoRollback -> sequence_ [
          pendingInUtxo l e w
        ]

      BasicRollback -> sequence_ [
        ]

      FullRollback -> sequence_ [
--          utxoExpectedDisjoint l e w
        ]

pendingInUtxo :: WalletInv h a
pendingInUtxo l e = invariant (l <> "/pendingInUtxo") e $ \w ->
    checkSubsetOf ("txIns (pending w)",
                    txIns (pending w))
                  ("utxoDomain (utxo w)",
                    utxoDomain (utxo w))

utxoIsOurs :: WalletInv h a
utxoIsOurs l e = invariant (l <> "/utxoIsOurs") e $ \w ->
    checkAllSatisfy ("isOurs",
                      ours w . outAddr)
                    ("utxoRange (utxo w)",
                      utxoRange (utxo w))

changeNotAvailable :: WalletInv h a
changeNotAvailable l e = invariant (l <> "/changeNotAvailable") e $ \w ->
    checkDisjoint ("utxoDomain (change w)",
                    utxoDomain (change w))
                  ("utxoDomain (available w)",
                    utxoDomain (available w))

changeNotInUtxo :: WalletInv h a
changeNotInUtxo l e = invariant (l <> "/changeNotInUtxo") e $ \w ->
    checkDisjoint ("utxoDomain (change w)",
                    utxoDomain (change w))
                  ("utxoDomain (utxo w)",
                    utxoDomain (utxo w))

changeAvailable :: WalletInv h a
changeAvailable l e = invariant (l <> "/changeAvailable") e $ \w ->
    checkEqual ("change w `utxoUnion` available w" ,
                 change w `utxoUnion` available w)
               ("total w",
                 total w)

balanceChangeAvailable :: WalletInv h a
balanceChangeAvailable l e = invariant (l <> "/balanceChangeAvailable") e $ \w ->
    checkEqual ("balance (change w) + balance (available w)",
                 balance (change w) + balance (available w))
               ("balance (total w)",
                 balance (total w))

{-
-- TODO: disabled until we fix the spec
utxoExpectedDisjoint :: WalletInv h a
utxoExpectedDisjoint l e = invariant (l <> "/utxoExpectedDisjoint") e $ \w ->
    checkDisjoint ("utxoDomain (utxo w)",
                    utxoDomain (utxo w))
                  ("utxoDomain (expectedUtxo w)",
                    utxoDomain (expectedUtxo w))
-}

{-------------------------------------------------------------------------------
  Compare different wallet implementations
-------------------------------------------------------------------------------}

walletEquivalent :: forall h a. (Hash h a, Eq a, Buildable a)
                 => Text
                 -> (Transaction h a -> Wallet h a)
                 -> (Transaction h a -> Wallet h a)
                 -> Invariant h a
walletEquivalent lbl e e' = void .
    interpret notChecked (\boot -> [e boot, e' boot]) p
  where
    notChecked :: OldestFirst [] (WalletEvent h a)
               -> InvalidInput h a
               -> InvariantViolation h a
    notChecked history reason = InvariantNotChecked {
          invariantNotCheckedName   = lbl
        , invariantNotCheckedReason = reason
        , invariantNotCheckedEvents = history
        }

    violation :: OldestFirst [] (WalletEvent h a)
              -> InvariantViolationEvidence
              -> InvariantViolation h a
    violation history ev = InvariantViolation {
          invariantViolationName     = lbl
        , invariantViolationEvidence = ev
        , invariantViolationEvents   = history
        }

    p :: OldestFirst [] (WalletEvent h a)
      -> [Wallet h a]
      -> Validated (InvariantViolation h a) ()
    p history [w, w'] = sequence_ [
          cmp "pending"          pending
        , cmp "utxo"             utxo
        , cmp "availableBalance" availableBalance
        , cmp "totalBalance"     totalBalance
        , cmp "available"        available
        , cmp "change"           change
        , cmp "total"            total
        ]
      where
        cmp :: (Eq b, Buildable b)
            => Text
            -> (Wallet h a -> b)
            -> Validated (InvariantViolation h a) ()
        cmp fld f =
          case checkEqual (fld <> " w", f w) (fld <> " w'", f w') of
            Nothing -> return ()
            Just ev -> throwError $ violation history ev
    p _ _ = error "impossible"

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (InvariantViolation h a) where
  build InvariantViolation{..} = bprint
    ( "InvariantViolation "
    % "{ name:     " % build
    % ", evidence: " % build
    % ", events:   " % build
    % "}"
    )
    invariantViolationName
    invariantViolationEvidence
    invariantViolationEvents
  build (InvariantNotChecked{..}) = bprint
    ( "InvariantNotChecked "
    % "{ name:   " % build
    % ", reason: " % build
    % ", events: " % build
    % "}"
    )
    invariantNotCheckedName
    invariantNotCheckedReason
    invariantNotCheckedEvents

instance Buildable InvariantViolationEvidence where
  build (NotEqual (labelX, x) (labelY, y)) = bprint
    ( "NotEqual "
    % "{ " % build % ": " % build
    % ", " % build % ": " % build
    % "}"
    )
    labelX
      x
    labelY
      y
  build (NotSubsetOf (labelXs, xs) (labelYs, ys)) = bprint
    ( "NotSubsetOf "
    % "{ " % build % ": " % listJson
    % ", " % build % ": " % listJson
    % ", " % build % ": " % listJson
    % "}"
    )
    labelXs
      (Set.toList xs)
    labelYs
      (Set.toList ys)
    (labelXs <> " \\\\ " <> labelYs)
      (Set.toList $ xs Set.\\ ys)
  build (NotAllSatisfy (labelP, p) (labelXs, xs)) = bprint
    ( "NotAllSatisfy "
    % "{ " % build % ": " % build
    % ", " % build % ": " % listJson
    % ", " % build % ": " % listJson
    % "}"
    )
    ("pred" :: Text)
      labelP
    labelXs
      xs
    ("filter (not . " <> labelP <> ")")
      (filter (not . p) xs)
  build (NotDisjoint (labelXs, xs) (labelYs, ys)) = bprint
    ( "NotSubsetOf "
    % "{ " % build % ": " % listJson
    % ", " % build % ": " % listJson
    % ", " % build % ": " % listJson
    % "}"
    )
    labelXs
      (Set.toList xs)
    labelYs
      (Set.toList ys)
    (labelXs <> " `intersection` " <> labelYs)
      (Set.toList $ xs `Set.intersection` ys)
