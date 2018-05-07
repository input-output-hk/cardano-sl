{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Abstract definition of a wallet
module Wallet.Abstract (
    -- * Abstract definition of a wallet
    Wallet(..)
  , Ours
  , Pending
  , WalletConstr
  , mkDefaultWallet
  , walletBoot
  , applyBlocks
    -- * Inductive wallet definition
  , Inductive(..)
  , interpret
    -- ** Invariants
  , Invariant
  , invariant
    -- ** Testing
  , walletInvariants
  , walletEquivalent
    -- ** Generation
    -- $generation
  , InductiveWithOurs(..)
  , genFromBlocktree
  , genFromBlocktreeWithOurs
  , genFromBlocktreePickingAccounts
    -- * Auxiliary operations
  , balance
  , txIns
  , txOuts
  , updateUtxo
  , updatePending
  , utxoRestrictToOurs
  ) where

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import           Universum

import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Text.Buildable
import           Formatting (bprint, build, sformat, (%))
import           Pos.Util.Chrono
                   (NewestFirst(NewestFirst), toNewestFirst,
                    OldestFirst(OldestFirst))
import           Pos.Util.QuickCheck.Arbitrary (sublistN)
import           Serokell.Util (listJson)
import           Test.QuickCheck

import           Util
import           Util.Validated
import           UTxO.BlockGen (selectDestination, estimateFee)
import           UTxO.Context
import           UTxO.Crypto
import           UTxO.DSL
import           UTxO.PreChain

{-------------------------------------------------------------------------------
  Wallet type class
-------------------------------------------------------------------------------}

-- | Check if an address is ours
type Ours a = a -> Maybe SomeKeyPair

-- | Pending transactions
type Pending h a = Set (Transaction h a)

-- | Abstract wallet interface
data Wallet h a = Wallet {
      -- MAIN API

      -- | Return the total balance of the wallet (see 'available')
      totalBalance :: Value

      -- | Return the available balance of the wallet (see 'total')
    , availableBalance :: Value

      -- | Notify the wallet of a new block
    , applyBlock :: Block h a -> Wallet h a

      -- | Submit a new transaction to be included in the blockchain
    , newPending :: Transaction h a -> Maybe (Wallet h a)

      -- | Rollback
    , rollback :: Wallet h a

      -- AUXILIARY API

      -- | Current set of pending transactions
    , pending :: Pending h a

      -- | Wallet's current UTxO (ignoring pending transactions)
    , utxo :: Utxo h a

      -- | Addresses that belong to the wallet
    , ours :: Ours a

      -- | Change from the pending transactions
    , change :: Utxo h a

      -- | Available UTxO
      --
      -- This is the UTxO with the inputs spent by the pending transactions
      -- removed.
    , available :: Utxo h a

      -- | Total UTxO
      --
      -- This is the available UTxO where we add back the change from the
      -- pending transactions.
    , total :: Utxo h a
    }

-- | Apply multiple blocks
applyBlocks :: Wallet h a -> Chain h a -> Wallet h a
applyBlocks w0 bs = foldl' applyBlock w0 bs

-- | Type of a wallet constructor
--
-- See <http://www.well-typed.com/blog/2018/03/oop-in-haskell/> for a
-- detailed discussion of the approach we take.
type WalletConstr h a st = (st -> Wallet h a) -> (st -> Wallet h a)

-- | Default wallet constructor
--
-- This does not pick any particular implementation, but provides some
-- default implementations of some of the wallet methods in terms of the
-- other methods.
mkDefaultWallet
  :: forall h a st
  .  (Hash h a, Ord a)
  => Lens' st (Pending h a)
  -> WalletConstr h a st
mkDefaultWallet l self st = Wallet {
      -- Dealing with pending
      pending    = st ^. l
    , newPending = \tx -> do
          -- Here we check that the inputs to the given transaction are a
          -- subset of the available unspent transaction outputs that aren't
          -- part of a currently pending transaction.
          let x = trIns tx :: Set (Input h a)
              y = utxoDomain (available this) :: Set (Input h a)
          case x `Set.isSubsetOf` y of
             True -> Just $ self (st & l %~ Set.insert tx)
             False -> Nothing
      -- UTxOs
    , available = utxoRemoveInputs (txIns (pending this)) (utxo this)
    , change    = utxoRestrictToOurs (ours this) (txOuts (pending this))
    , total     = available this `utxoUnion` change this
      -- Balance
    , availableBalance = balance $ available this
    , totalBalance     = balance $ total     this
      -- Functions without a default
    , utxo       = error "mkDefaultWallet: no default for utxo"
    , ours       = error "mkDefaultWallet: no default for ours"
    , applyBlock = error "mkDefaultWallet: no default for applyBlock"
    , rollback   = error "mkDefaultWallet: no default for rollback"
    }
  where
    this = self st

-- | Wallet state after the bootstrap transaction
walletBoot :: (Ours a -> Wallet h a) -- ^ Wallet constructor
           -> Ours a -> Transaction h a -> Wallet h a
walletBoot mkWallet p boot = applyBlock (mkWallet p) (OldestFirst [boot])

{-------------------------------------------------------------------------------
  Inductive wallet definition
-------------------------------------------------------------------------------}

-- | Inductive definition of a wallet
data Inductive h a =
    -- | Start the wallet, given the bootstrap transaction
    WalletBoot (Transaction h a)

    -- | Inform the wallet of a new block added to the blockchain
  | ApplyBlock (Inductive h a) (Block h a)

    -- | Submit a new transaction to the wallet to be included in the blockchain
  | NewPending (Inductive h a) (Transaction h a)

    -- | Roll back the last block added to the blockchain
  | Rollback (Inductive h a)
  deriving Eq

-- | Interpreter for 'Inductive'
--
-- Given (one or more) wallet constructors, evaluate an 'Inductive' wallet,
-- checking the given property at each step.
--
-- Note: we expect the 'Inductive' to be valid (valid blockchain, valid
-- calls to 'newPending', etc.). This is meant to check properties of the
-- /wallet/, not the wallet input.
interpret :: forall h a err.
             (Inductive h a -> InvalidInput h a -> err)
          -- ^ Inject invalid input err. We provide the value of the
          -- 'Inductive' at the point of the error.
          -> (Transaction h a -> [Wallet h a])
          -- ^ Wallet constructors
          -> (Inductive h a -> [Wallet h a] -> Validated err ())
          -- ^ Predicate to check. The predicate is passed the 'Inductive'
          -- at the point of the error, for better error messages.
          -> Inductive h a
          -- ^ 'Inductive' value to interpret
          -> Validated err [Wallet h a]
interpret invalidInput mkWallets p = fmap snd . go
  where
    go :: Inductive h a -> Validated err (Ledger h a, [Wallet h a])
    go ind@(WalletBoot t) = do
      ws <- verify ind (mkWallets t)
      return (ledgerSingleton t, ws)
    go ind@(ApplyBlock w b) = do
      (l, ws) <- go w
      ws' <- verify ind $ map (`applyBlock` b) ws
      return (ledgerAdds (toNewestFirst b) l, ws')
    go ind@(NewPending w t) = do
      (l, ws) <- go w
      ws' <- verify ind =<< mapM (verifyNew ind l t) ws
      return (l, ws')
    go (Rollback w) = go w

    verify :: Inductive h a -> [Wallet h a] -> Validated err [Wallet h a]
    verify ind ws = p ind ws >> return ws

    -- Verify the input
    -- If this fails, we provide the /entire/ 'Inductive' value so that it's
    -- easier to track down what happened.
    verifyNew :: Inductive h a -- ^ Inductive value at this point
              -> Ledger h a    -- ^ Ledger so far (for error messages)
              -> Transaction h a -> Wallet h a -> Validated err (Wallet h a)
    verifyNew ind l tx w =
        case newPending w tx of
          Just w' -> return w'
          Nothing -> throwError . invalidInput ind
                   $ InvalidPending tx (utxo w) (pending w) l

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
    notChecked :: Inductive h a
               -> InvalidInput h a
               -> InvariantViolation h a
    notChecked ind reason = InvariantNotChecked {
          invariantNotCheckedName      = name
        , invariantNotCheckedReason    = reason
        , invariantNotCheckedInductive = ind
        }

    violation :: Inductive h a
              -> InvariantViolationEvidence
              -> InvariantViolation h a
    violation ind ev = InvariantViolation {
          invariantViolationName      = name
        , invariantViolationEvidence  = ev
        , invariantViolationInductive = ind
        }

    p' :: Inductive h a
       -> [Wallet h a]
       -> Validated (InvariantViolation h a) ()
    p' ind [w] = case p w of
                   Nothing -> return ()
                   Just ev -> throwError (violation ind ev)
    p' _ _ = error "impossible"

-- | Invariant violation
data InvariantViolation h a =
    -- | Invariance violation
    InvariantViolation {
        -- | Name of the invariant
        invariantViolationName :: Text

        -- | Evidence that the invariant was violated
      , invariantViolationEvidence :: InvariantViolationEvidence

        -- | The 'Inductive' value at the point of the error
      , invariantViolationInductive :: Inductive h a
      }

    -- | The invariant was not checked because the input was invalid
  | InvariantNotChecked {
        -- | Name of the invariant
        invariantNotCheckedName :: Text

        -- | Why did we not check the invariant
      , invariantNotCheckedReason :: InvalidInput h a

        -- | The 'Inductive' value at the point of the error
      , invariantNotCheckedInductive :: Inductive h a
      }

-- | We were unable to check the invariant because the input was invalid
--
-- This indicates a bug in the generator (or in the hand-written 'Inductive'),
-- so we try to provide sufficient information to track that down.
data InvalidInput h a =
    InvalidPending {
        -- | The submitted transaction that was invalid
        invalidPendingTransaction :: Transaction h a

        -- | The UTxO of the wallet at the time of submission
      , invalidPendingWalletUtxo :: Utxo h a

        -- | The pending set of the wallet at time of submission
      , invalidPendingWalletPending :: Pending h a

        -- | The ledger seen so far at the time of submission
      , invalidPendingLedger :: Ledger h a
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

walletInvariants :: WalletInv h a
walletInvariants l e w = sequence_
    [ pendingInUtxo          l e w
    , utxoIsOurs             l e w
    , changeNotAvailable     l e w
    , changeNotInUtxo        l e w
    , changeAvailable        l e w
    , balanceChangeAvailable l e w
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
                      isJust . ours w . outAddr)
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
    notChecked :: Inductive h a
               -> InvalidInput h a
               -> InvariantViolation h a
    notChecked ind reason = InvariantNotChecked {
          invariantNotCheckedName      = lbl
        , invariantNotCheckedReason    = reason
        , invariantNotCheckedInductive = ind
        }

    violation :: Inductive h a
              -> InvariantViolationEvidence
              -> InvariantViolation h a
    violation ind ev = InvariantViolation {
          invariantViolationName      = lbl
        , invariantViolationEvidence  = ev
        , invariantViolationInductive = ind
        }

    p :: Inductive h a
      -> [Wallet h a]
      -> Validated (InvariantViolation h a) ()
    p ind [w, w'] = sequence_ [
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
            Just ev -> throwError $ violation ind ev
    p _ _ = error "impossible"

{-------------------------------------------------------------------------------
  Auxiliary operations
-------------------------------------------------------------------------------}

balance :: Utxo h a -> Value
balance = sum . map outVal . Map.elems . utxoToMap

txIns :: (Hash h a, Foldable f) => f (Transaction h a) -> Set (Input h a)
txIns = Set.unions . map trIns . Fold.toList

txOuts :: (Hash h a, Foldable f) => f (Transaction h a) -> Utxo h a
txOuts = utxoUnions . map trUtxo . Fold.toList

updateUtxo :: forall h a. Hash h a
           => Ours a -> Block h a -> Utxo h a -> Utxo h a
updateUtxo p b = remSpent . addNew
  where
    addNew, remSpent :: Utxo h a -> Utxo h a
    addNew   = utxoUnion (utxoRestrictToOurs p (txOuts b))
    remSpent = utxoRemoveInputs (txIns b)

updatePending :: forall h a. Hash h a => Block h a -> Pending h a -> Pending h a
updatePending b = Set.filter $ \t -> disjoint (trIns t) (txIns b)

utxoRestrictToOurs :: Ours a -> Utxo h a -> Utxo h a
utxoRestrictToOurs p = utxoRestrictToAddr (isJust . p)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- $generation
--
-- The 'Inductive' data type describes a potential history of a wallet's
-- view of an existing blockchain. This means that there are many possible
-- 'Inductive's for any given blockchain -- any set of addresses can belong
-- to the 'Inductive' that the wallet is for, and there are many possible
-- sequences of actions that adequately describe the view of the
-- blockchain.

-- | A monad for generating inductive chains.
newtype InductiveGen h a
    = InductiveGen
    { unInductiveGen :: ReaderT (InductiveCtx h) Gen a
    } deriving (Functor, Applicative, Monad, MonadReader (InductiveCtx h))

runInductiveGen :: FromPreTree h () -> InductiveGen h a -> Gen a
runInductiveGen fpc ig = runReaderT (unInductiveGen ig) (initializeCtx fpc)

data InductiveCtx h
    = InductiveCtx
    { icFromPreTree  :: !(FromPreTree h ())
    }

initializeCtx :: FromPreTree h () -> InductiveCtx h
initializeCtx fpc = InductiveCtx{..}
  where
    icFromPreTree = fpc

getFromPreTree :: InductiveGen h (FromPreTree h ())
getFromPreTree = asks icFromPreTree

getBlocktree :: InductiveGen h (BlockTree h Addr)
getBlocktree = fptTree <$> getFromPreTree

getBootTransaction :: InductiveGen h (Transaction h Addr)
getBootTransaction = fptBoot <$> getFromPreTree

-- | The 'Inductive' data type is isomorphic to a linked list of this
-- 'Action' type. It is more convenient to operate on this type, as it can
-- vary the sequence representation and reuse sequence functions.
data Action h a
    = ApplyBlock' (Block h a)
    | NewPending' (Transaction h a)
    | Rollback'

-- | Smart constructor that adds the callstack to the transaction's comments
-- (Useful for finding out where transactions are coming from)
newPending' :: HasCallStack => [Text] -> Transaction h a -> Action h a
newPending' extra t = NewPending' (t { trExtra = trExtra t ++ extra })

-- | Convert a container of 'Action's into an 'Inductive' wallet,
-- given the bootstrap transaction.
toInductive :: (Hash h a, Buildable a) => Transaction h a -> [Action h a] -> Inductive h a
toInductive boot = foldl' k (WalletBoot boot)
  where
    k acc (ApplyBlock' a) = ApplyBlock acc a
    k acc (NewPending' a) = NewPending acc a
    k acc Rollback' = Rollback acc

-- | Given a 'Set' of addresses that will represent the addresses that
-- belong to the generated 'Inductive' wallet and the 'FromPreChain' value
-- that contains the relevant blockchain, this will be able to generate
-- arbitrary views into the blockchain.
genFromBlocktree
    :: Hash h Addr
    => Set Addr
    -> FromPreTree h ()
    -> Gen (Inductive h Addr)
genFromBlocktree addrs fpc =
    runInductiveGen fpc (genInductiveFor addrs)

-- | Pair an 'Inductive' wallet definition with the set of addresses owned
data InductiveWithOurs h a = InductiveWithOurs {
      inductiveWalletOurs :: Set a
    , inductiveWalletDef  :: Inductive h a
    }

;
-- | Given a predicate function that selects addresses that
-- belong to the generated 'Inductive' wallet and the 'FromPreChain' value
-- that contains the relevant blockchain, this will build a set of addrs and
-- call genFromBlocktree
genFromBlocktreeWithOurs
    :: Hash h Addr
    => (Addr -> Bool)
    -> FromPreTree h ()
    -> Gen (InductiveWithOurs h Addr)
genFromBlocktreeWithOurs isOurs fpt
     | null ourAddrs =
        error $ sformat
            ( "None of the addresses are ours!\n\n"
            % "All addresses: " % build
            ) (intercalate ", " (map show (Set.toList allAddrs)))
     | otherwise =
        InductiveWithOurs ourAddrs <$> genFromBlocktree ourAddrs fpt
  where
    allAddrs, ourAddrs :: Set Addr
    allAddrs = fptAddresses fpt
    ourAddrs = Set.filter isOurs allAddrs

-- | Selects a random subset of addresses to be considered from the
-- blockchain in the amount given.
genFromBlocktreePickingAccounts
    :: Hash h Addr
    => Int
    -> FromPreTree h ()
    -> Gen (InductiveWithOurs h Addr)
genFromBlocktreePickingAccounts i fpc = do
    let allAddrs = toList (fptAddresses fpc)
        eligibleAddrs = filter (not . isAvvmAddr) allAddrs

    if null eligibleAddrs then
        error
        $ sformat
            ( "No eligible addresses!\n\n"
            % "All addresses: " % build
            ) (intercalate ", " (map show allAddrs))
        else pure ()

    addrs <- Set.fromList <$> sublistN i eligibleAddrs

    if null addrs then
        error
        $ sformat
            ( "No addresses!\n\n"
            % "All addresses: " % build
            ) (intercalate ", " (map show allAddrs))
        else pure ()

    InductiveWithOurs addrs <$> genFromBlocktree addrs fpc

genInductiveFor :: Hash h Addr => Set Addr -> InductiveGen h (Inductive h Addr)
genInductiveFor addrs = do
    boot <- getBootTransaction
    tree <- getBlocktree
    intersperseTransactions boot addrs (treeToApplyBlocks tree)

-- | Linearise a blocktree to a list of actions.
--
--   Because we construct the tree to have the principal branch on the RHS, this
--   is just given by the pre-order traversal, except we record Rollbacks any time
--   we backtrack.
treeToApplyBlocks :: BlockTree h a -> [Action h a]
treeToApplyBlocks (OldestFirst root) = reverse . fst $ go root ([], 1)
  where
    treeHeight = length $ Tree.levels root
    go (Tree.Node val []) (acc, lvl)
      | lvl == treeHeight = (ApplyBlock' val : acc, lvl)
      | otherwise = (Rollback' : ApplyBlock' val : acc, lvl - 1)
    go (Tree.Node val xs) (acc, lvl) =
      foldr go (ApplyBlock' val : acc, lvl + 1) xs


blocksToLedger :: Chain h a -> Ledger h a
blocksToLedger blocks = Ledger $ NewestFirst $ do
  block <- reverse (toList blocks)
  reverse (toList block)

actionsToBlocks
  :: forall h a
  .  Transaction h a   -- ^ Boot transaction
  -> [Action h a]
  -> Chain h a
actionsToBlocks boot =
    OldestFirst . reverse . map OldestFirst . foldl' f [[boot]]
  where
    f :: [[Transaction h a]] -> Action h a -> [[Transaction h a]]
    -- Ordering as in `NewestFirst [] (OldestFirst [] (Transaction h a))`
    f xs (ApplyBlock' (OldestFirst x)) = x : xs
    f [] Rollback' = error "actionsToBlocks: can't rollback"
    f (_ : xs) Rollback' = xs
    f _xs (NewPending' _) = error "actionsToBlocks: NewPending'" -- _xs



-- | Once we've created our initial @['Action' h 'Addr']@, we want to
-- insert some 'Transaction's in appropriate locations in the list. There
-- are some properties that the inserted events must satisfy:
--
-- * The transaction must be after all of the blocks that confirm inputs to
--   the transaction.
-- * The transaction must be before the block that confirms it, if any
--   blocks confirm it. It is not necessary that the transaction gets
--   confirmed eventually!
--
-- See Note [Intersperse]
intersperseTransactions
    :: forall h
    .  Hash h Addr
    => Transaction h Addr -- ^ Bootstrap transaction
    -> Set Addr           -- ^ " Our " addresses
    -> [Action h Addr]    -- ^ Initial actions (the blocks in the chain),
                          --   not including bootstrap transaction.
    -> InductiveGen h (Inductive h Addr)
intersperseTransactions boot addrs actions = do
    let blocks :: Chain h Addr = actionsToBlocks boot actions
    -- Transactions that use outputs listed in `addr`, together with the
    -- block index when those outputs were confirmed.
    let ourTxns :: [(Int, Transaction h Addr)]
          = findOurTransactions addrs blocks
    -- We weigh the frequency distribution such that most of the
    -- transactions will be represented by this wallet. This can be
    -- changed or made configurable later.
    txnToDisperseCount :: Int <- case length ourTxns of
       n -> liftGen $ frequency (zip [1 .. n+1] (map pure [0 .. n]))
    -- Subset of @txnsToDisperse@ where each tuple is accompanied by the block
    -- index where the transactions are confirmed, which is always larger
    -- than the index where the transaction input's outputs were confirmed.
    -- Unconfirmed transactions are not included in this list.
    txnsWithRange :: [((Int, Transaction h Addr), Int)] <- do
       txnsToDisperse <- liftGen $ sublistN txnToDisperseCount ourTxns
       pure $ do
          (i, txn) <- txnsToDisperse
          when (i == 0) (error "txnsWithRange: i == 0")
          case transactionFullyConfirmedAt addrs txn blocks of
             Just k | i < k -> [((i, txn), k)]
                    | otherwise -> [] -- transaction confirmed in same block.
             Nothing -> [] -- transaction not confirmed.
    -- New transactions to insert, and the block index at which to insert them.
    txnsWithIndex :: [(Int, Transaction h Addr)] <- do
       fmap join $ forM txnsWithRange $ \((lo, t), hi) -> do
          i <- liftGen $ choose (lo, hi - 1)
          let fmt = "Inserted at " % build % " <= " % build % " < " % build
              t' = t { trExtra = sformat fmt lo i hi : trExtra t }
          pure [(i, t')]
    -- 'NewPending'' actions for known-to-be-confirmed transactions to insert
    -- at the block indexes identified by 'IntMap.Key'
    let confirmed :: IntMap [Action h Addr] = foldr
          (\(i, t) -> IntMap.insertWith (flip (<>)) i [newPending' [] t])
          (dissect actions)
          txnsWithIndex
    -- Inputs already spent.
    let spent :: Set (Input h Addr) = mconcat
          [ -- Inputs being spent by the new transactions in `confirmed`.
            Set.unions (map (trIns . snd) txnsWithIndex)
          , -- Inputs that were present in `blocks` from before.
            -- TODO: Listing these might be unecessary because we already
            -- whitelist by `addr` in `synthesizeTransactions`.
            utxoDomain (ledgerUtxo (blocksToLedger blocks)) ]
    -- 'NewPending'' actions for known-to-be-unconfirmed transactions to insert
    -- at the block indexes identified by 'IntMap.Key'
    unconfirmed :: IntMap [Action h Addr] <-
       synthesizeTransactions addrs blocks spent
    pure $ toInductive boot
         $ conssect (IntMap.unionWith (<>) confirmed unconfirmed)

-- | Generate transactions that will never be confirmed
--
-- We take as argument the set of inputs already spent by transactions that
-- /will/ be confirmed, so that we don't create an 'Inductive' wallet with
-- double spending.
synthesizeTransactions
    :: forall h. Hash h Addr
    => Set Addr           -- ^ Addresses owned by the wallet
    -> Chain h Addr       -- ^ Blockchain (including boot)
    -> Set (Input h Addr) -- ^ Inputs already spent
    -> InductiveGen h (IntMap [Action h Addr])
synthesizeTransactions addrs blocks alreadySpent = do
    let nextHash :: Int = minBound  -- negative hashes are not used elsewhere.
    liftGen $ go IntMap.empty nextHash (Utxo mempty) alreadySpent 0 blocks
  where
    -- NOTE: We maintain a UTxO as we process the blocks. There are (at least)
    -- two alternatives here:
    --
    -- * Depend on a wallet implementation to keep track both of the utxo and
    --   of the set of pending transactions. That would make the tests
    --   kind of circular though (using the wallet to test the wallet);
    --   better to keep the two independent.
    -- * We could reuse the UTxOs that we compute while we compute the chain.
    --   This is certainly a possibility, /but/ there is a downside there:
    --   those UTxOs are approximate only, as we don't have any fees.
    --   The chain as we have it here has proper fee values.
    --   (Having said that, we still don't have proper fee values for the
    --   transactions we generate here.)
    go :: IntMap [Action h Addr]  -- Accumulator
       -> Int                     -- Next txn hash to use if necessary.
       -> Utxo h Addr             -- Current utxo
       -> Set (Input h Addr)      -- All inputs already spent
       -> Int                     -- Block index of head of the given 'Blocks'.
       -> Chain h Addr            -- Chain yet to process
       -> Gen (IntMap [Action h Addr])
    go _ 0 _ _ _ _ =
      -- We don't deal with this very unlikely scenario in tests, but we fail
      -- verbosely if it happens.
      error "synthesizeTransactions created too many transactions"
    go acc _ _ _ _ (OldestFirst []) =
      -- We could create some pending transactions after the very last block,
      -- but we don't
      return acc
    go acc nextHash utxoBefore spent ix (OldestFirst (b:bs)) = do
      let -- In `utxoAfter` we extend `utxoBefore` to also include those UTxOs
          -- generated by `b`.
          utxoAfter :: Utxo h Addr = utxoApplyBlock b utxoBefore
          -- In `utxoAvail` we only keep those UTxOs whose inputs come from our
          -- own addresses and have not yet been spent.
          utxoAvail :: Utxo h Addr
             = utxoRemoveInputs spent $
               utxoRestrictToAddr (flip Set.member addrs) $
               utxoAfter
      pct <- choose (0, 100 :: Int)
      if pct >= 5 || utxoNull utxoAvail
        then do
          -- 95% of the time (or if there are no UTxOs available for us to use
          -- as input), we skip adding new transactions to this block.
          go acc nextHash utxoAfter spent (ix + 1) (OldestFirst bs)
        else do
          -- 5% of the time, we create a new transaction using one of UTxOs in
          -- `utxoAvail` as input:
          (i, o) <- elements $ utxoToList utxoAvail
          -- ... and a not yet spent addresses in `utxoAfter` as output:
          dest :: Addr <- selectDestination (utxoRemoveInputs spent utxoAfter)
          let txn :: Transaction h Addr = Transaction
                  { trFresh = 0
                  , trFee   = estimateFee txn -- lazy enough, doesn't diverge
                  , trHash  = nextHash
                  , trIns   = Set.singleton i
                  , trOuts  = [Output dest (outVal o)]
                  , trExtra = [] }
              act :: Action h Addr = newPending' ["never confirmed"] txn
          go (IntMap.insert ix [act] acc)
             (nextHash + 1)
             utxoAfter
             (Set.insert i spent)
             (ix + 1)
             (OldestFirst bs)

-- | Construct an 'IntMap' consisting of the index of the element in the
-- input list pointing to a singleton list of the element the original
-- list.
dissect :: [a] -> IntMap [a]
dissect = IntMap.fromList . zip [0..] . map pure

-- | Reverse the operation of 'dissect'. Given an 'IntMap' originally
-- representing the original index in the list pointing to the list of new
-- items at that starting index, collapse that into a single list of
-- elements.
conssect :: IntMap [a] -> [a]
conssect = concatMap snd . IntMap.toList


-- | Given a set of addresses and 'Blocks' (including the boot
-- transaction), return all of the 'Transaction's whose output includes at
-- least one of said addresses together with the block index when that 'Output'
-- was confirmed.
findOurTransactions
  :: forall h a
  .  (Hash h a, Ord a)
  => Set a
  -> Chain h a -- ^ Includes boot transaction.
  -> [(Int, Transaction h a)] -- ^ Block index, transaction.
findOurTransactions addrs blocks = do
  let ledger = blocksToLedger blocks
  (n, block) <- zip [0..] (toList blocks)
  tr <- toList block
  guard $ or $ do
     inp <- trIns' tr
     case inpSpentOutput inp ledger of
       Just o -> [Set.member (outAddr o) addrs]
       Nothing -> []
  pure (n, tr)

-- | This function identifies the index of the block that the input was
-- received in the ledger, marking the point at which it may be inserted as
-- a 'NewPending' transaction.
blockReceivedIndex :: Hash h Addr => Input h Addr -> Chain h Addr -> Maybe Int
blockReceivedIndex i = List.findIndex (any ((inpTrans i ==) . hash)) . toList

-- | For each 'Input' in the 'Transaction' that belongs to one of the
-- 'Addr'esses in the 'Set' provided, find the index of the block in the
-- 'Chain' that confirms that 'Input'. Take the maximum index and return
-- that -- that is the earliest this transaction may appear as a pending
-- transaction.
transactionFullyConfirmedAt
  :: forall h
  .  Hash h Addr
  => Set Addr
  -> Transaction h Addr
  -> Chain h Addr
  -> Maybe Int
transactionFullyConfirmedAt addrs txn blocks = do
  let ledger :: Ledger h Addr = blocksToLedger blocks
  foldl' max Nothing $ do
     inp :: Input h Addr <- trIns' txn
     case inpSpentOutput inp ledger of
        Just o | Set.member (outAddr o) addrs ->
           [blockReceivedIndex inp blocks]
        _ -> []

liftGen :: Gen a -> InductiveGen h a
liftGen = InductiveGen . lift

{- Note [Intersperse]
~~~~~~~~~~~~~~~~~~~~~
Given a list of blocks

> [ applyBlock_0, applyBlock_1, applyBlock_2, applyBlock_3, applyBlock_4 ]

we construct an 'IntMap' out of them where the index in the intmap is the
original index of that block in the chain:

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1]
> , 2 -> [applyBlock_2]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

Then, when we select an index between @lo@ (the latest block to confirm an input
in the transaction) and @hi@ (the index of the block that confirms the
transaction itself), we can 'insertWith' at that index. Suppose we have a
transaction @t@ where the input was provided in block 1 and is confirmed in
block 3. That means we can have @NewPending t@ inserted into either index 1 or
2:

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1] <> [newPending t] = [applyBlock_1, newPending t]
> , 2 -> [applyBlock_2]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

or

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1]
> , 2 -> [applyBlock_2] <> [newPending t] = [applyBlock_2, newPending t]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

Then, when we finally go to 'conssec' the @IntMap [Action h a]@ back into a
@[Action h a]@, we get:

> [ applyBlock_0
> , applyBlock_1
> , applyBlock_2, newPending t
> , applyBlock_3
> , applyBlock_4
> ]

TODO: This means that currently we never insert pending transactions before
the first block after boot.
-}

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (Pending h a) where
  build = bprint listJson . Set.toList

instance (Hash h a, Buildable a) => Buildable (InvalidInput h a) where
  build InvalidPending{..} = bprint
    ( "InvalidPending "
    % "{ transaction:   " % build
    % ", walletUtxo:    " % build
    % ", walletPending: " % build
    % ", ledger:        " % build
    % "}"
    )
    invalidPendingTransaction
    invalidPendingWalletUtxo
    invalidPendingWalletPending
    invalidPendingLedger

instance (Hash h a, Buildable a) => Buildable (InvariantViolation h a) where
  build InvariantViolation{..} = bprint
    ( "InvariantViolation "
    % "{ name:      " % build
    % ", evidence:  " % build
    % ", inductive: " % build
    % "}"
    )
    invariantViolationName
    invariantViolationEvidence
    invariantViolationInductive
  build (InvariantNotChecked{..}) = bprint
    ( "InvariantNotChecked "
    % "{ name:      " % build
    % ", reason:    " % build
    % ", inductive: " % build
    % "}"
    )
    invariantNotCheckedName
    invariantNotCheckedReason
    invariantNotCheckedInductive

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

-- | We output the inductive in the order that things are applied; something like
--
-- > { "boot": <boot transaction>
-- > , "block": <first block>
-- > , "new": <first transaction>
-- > , "block": <second block>
-- > ..
-- > }
instance (Hash h a, Buildable a) => Buildable (Inductive h a) where
  build ind = bprint (build % "}") (go ind)
    where
      go (WalletBoot   t) = bprint (        "{ boot:     " % build)        t
      go (ApplyBlock n b) = bprint (build % ", block:    " % build) (go n) b
      go (NewPending n t) = bprint (build % ", new:      " % build) (go n) t
      go (Rollback   n  ) = bprint (build % ", rollback: "        ) (go n)

instance (Hash h a, Buildable a) => Buildable (Action h a) where
  build (ApplyBlock' b) = bprint ("ApplyBlock' " % build) b
  build (NewPending' t) = bprint ("NewPending' " % build) t
  build Rollback'       = bprint "Rollback'"

instance (Hash h a, Buildable a) => Buildable [Action h a] where
  build = bprint listJson

instance (Hash h a, Buildable a) => Buildable (InductiveWithOurs h a) where
  build InductiveWithOurs{..} = bprint
    ( "InductiveWithOurs"
    % "{ ours: " % listJson
    % ", def:  " % build
    % "}"
    )
    inductiveWalletOurs
    inductiveWalletDef
