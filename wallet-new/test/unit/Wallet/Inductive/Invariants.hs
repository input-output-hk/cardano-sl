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

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)

import           Cardano.Wallet.Kernel.Util (disjoint)

import           Util.Validated
import           UTxO.DSL
import           Wallet.Abstract
import           Wallet.Inductive
import           Wallet.Inductive.History
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
invariant :: forall h a. (Hash h a, Buildable a)
          => Text                             -- ^ Name of the invariant
          -> (Transaction h a -> Wallet h a)  -- ^ Construct empty wallet
          -> (Wallet h a -> Maybe InvariantViolationEvidence) -- ^ Property
          -> Invariant h a
invariant name e p = void . interpret notChecked ((:[]) . e) p'
  where
    notChecked :: History
               -> InvalidInput h a
               -> InvariantViolation h a
    notChecked history reason = InvariantNotChecked name reason history

    violation :: History
              -> InvariantViolationEvidence
              -> InvariantViolation h a
    violation history ev = InvariantViolation name ev history

    p' :: History
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
      , invariantViolationEvents   :: History
      }

    -- | The invariant was not checked because the input was invalid
  | InvariantNotChecked {
        -- | Name of the invariant
        invariantNotCheckedName   :: Text

        -- | Why did we not check the invariant
      , invariantNotCheckedReason :: InvalidInput h a

        -- | The events that led to the error
      , invariantNotCheckedEvents :: History
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
checkEqual = checkEqualUsing identity

checkEqualUsing :: (Buildable a, Eq b)
                => (a -> b) -> (Text, a) -> (Text, a) -> Maybe InvariantViolationEvidence
checkEqualUsing f (labelX, x) (labelY, y) =
    if f x == f y
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
type WalletInv h a = Text -> (Transaction h a -> Wallet h a) -> Invariant h a

-- | Which invariants are applicable to this wallet?
data ApplicableInvariants =
    -- | There are no rollbacks in the system
    NoRollback

    -- | We have rollbacks, but we are working in the basic model
  | BasicRollback

    -- | We are working in the full model (including support for expected UTxO)
  | FullRollback

walletInvariants :: (Hash h a, Buildable a, Eq a) => ApplicableInvariants -> WalletInv h a
walletInvariants applicableInvariants l e w = do
    -- Invariants applicable in all models
    sequence_ [
        utxoIsOurs             l e w
      , changeNotAvailable     l e w
      , changeNotInUtxo        l e w
      , changeAvailable        l e w
      , balanceChangeAvailable l e w
      , pendingInputsDisjoint  l e w
      ]

    case applicableInvariants of
      -- Invariants that only hold when there are no rollbacks
      NoRollback -> sequence_ [
          pendingInUtxo l e w
        ]

      BasicRollback -> sequence_ [
        ]

      FullRollback -> sequence_ [
          utxoExpectedDisjoint    l e w
        , expectedUtxoIsOurs      l e w
        , pendingInUtxoOrExpected l e w
        ]

pendingInUtxo :: (Hash h a, Buildable a) => WalletInv h a
pendingInUtxo l e = invariant (l <> "/pendingInUtxo") e $ \w ->
    checkSubsetOf ("txIns (pending w)",
                    txIns (pending w))
                  ("utxoDomain (utxo w)",
                    utxoDomain (utxo w))

utxoIsOurs :: (Hash h a, Buildable a) => WalletInv h a
utxoIsOurs l e = invariant (l <> "/utxoIsOurs") e $ \w ->
    checkAllSatisfy ("isOurs",
                      ours w . outAddr)
                    ("utxoRange (utxo w)",
                      utxoRange (utxo w))

changeNotAvailable :: (Hash h a, Buildable a) => WalletInv h a
changeNotAvailable l e = invariant (l <> "/changeNotAvailable") e $ \w ->
    checkDisjoint ("utxoDomain (change w)",
                    utxoDomain (change w))
                  ("utxoDomain (available w)",
                    utxoDomain (available w))

changeNotInUtxo :: (Hash h a, Buildable a) => WalletInv h a
changeNotInUtxo l e = invariant (l <> "/changeNotInUtxo") e $ \w ->
    checkDisjoint ("utxoDomain (change w)",
                    utxoDomain (change w))
                  ("utxoDomain (utxo w)",
                    utxoDomain (utxo w))

changeAvailable :: (Hash h a, Buildable a, Eq a) => WalletInv h a
changeAvailable l e = invariant (l <> "/changeAvailable") e $ \w ->
    checkEqual ("change w `utxoUnion` available w" ,
                 change w `utxoUnion` available w)
               ("total w",
                 total w)

balanceChangeAvailable :: (Hash h a, Buildable a) => WalletInv h a
balanceChangeAvailable l e = invariant (l <> "/balanceChangeAvailable") e $ \w ->
    checkEqual ("utxoBalance (change w) + utxoBalance (available w)",
                 utxoBalance (change w) + utxoBalance (available w))
               ("utxoBalance (total w)",
                 utxoBalance (total w))

pendingInputsDisjoint :: (Hash h a, Buildable a) => WalletInv h a
pendingInputsDisjoint l e = invariant (l <> "/pendingInputsDisjoint") e $ \w ->
    asum [ checkDisjoint ("trIns " <> pretty h1, trIns tx1)
                         ("trIns " <> pretty h2, trIns tx2)
         | (h1, tx1) <- Map.toList $ pending w
         , (h2, tx2) <- Map.toList $ pending w
         , h1 /= h2
         ]

utxoExpectedDisjoint :: (Hash h a, Buildable a) => WalletInv h a
utxoExpectedDisjoint l e = invariant (l <> "/utxoExpectedDisjoint") e $ \w ->
    checkDisjoint ("utxoDomain (utxo w)",
                    utxoDomain (utxo w))
                  ("utxoDomain (expectedUtxo w)",
                    utxoDomain (expectedUtxo w))

expectedUtxoIsOurs :: (Hash h a, Buildable a) => WalletInv h a
expectedUtxoIsOurs l e = invariant (l <> "/expectedUtxoIsOurs") e $ \w ->
    checkAllSatisfy ("isOurs",
                      ours w . outAddr)
                    ("utxoRange (expectedUtxo w)",
                      utxoRange (expectedUtxo w))

pendingInUtxoOrExpected :: (Hash h a, Buildable a) => WalletInv h a
pendingInUtxoOrExpected l e = invariant (l <> "/pendingInUtxoOrExpected") e $ \w ->
    checkSubsetOf ("txIns (pending w)",
                    txIns (pending w))
                  ("utxoDomain (utxo w) `Set.union` utxoDomain (expectedUtxo w)",
                    utxoDomain (utxo w) `Set.union` utxoDomain (expectedUtxo w))

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
    notChecked :: History
               -> InvalidInput h a
               -> InvariantViolation h a
    notChecked history reason = InvariantNotChecked lbl reason history

    violation :: History
              -> InvariantViolationEvidence
              -> InvariantViolation h a
    violation history ev = InvariantViolation lbl ev history

    p :: History
      -> [Wallet h a]
      -> Validated (InvariantViolation h a) ()
    p history [w, w'] = sequence_ [
          cmp "pending"          pending          Map.keys
        , cmp "utxo"             utxo             identity
        , cmp "availableBalance" availableBalance identity
        , cmp "totalBalance"     totalBalance     identity
        , cmp "available"        available        identity
        , cmp "change"           change           identity
        , cmp "total"            total            identity
        ]
      where
        cmp :: (Buildable b, Eq c)
            => Text              -- label
            -> (Wallet h a -> b) -- field to compare
            -> (b -> c)          -- what part of the field to compare
            -> Validated (InvariantViolation h a) ()
        cmp fld f g =
          case checkEqualUsing g (fld <> " w", f w) (fld <> " w'", f w') of
            Nothing -> return ()
            Just ev -> throwError $ violation history ev
    p _ _ = error "impossible"

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (InvariantViolation h a) where
  build (InvariantViolation
             invariantViolationName
             invariantViolationEvidence
             invariantViolationEvents) = bprint
    ( "InvariantViolation "
    % "{ name:     " % build
    % ", evidence: " % build
    % ", events:   " % build
    % "}"
    )
    invariantViolationName
    invariantViolationEvidence
    invariantViolationEvents
  build (InvariantNotChecked
             invariantNotCheckedName
             invariantNotCheckedReason
             invariantNotCheckedEvents) = bprint
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
