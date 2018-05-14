{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Wallet.Inductive.Generator (
    -- * Wallet event tree
    -- ** Parameters
    GenEventsParams(..)
  , defEventsParams
    -- ** State
  , GenEventsGlobalState(..)
  , gegsNextHash
  , gegsPending
  , initEventsGlobalState
    -- ** Generator
  , GenEvents
  , genEventTree
    -- * Wallet events
  , genWalletEvents
  ) where

import           Universum

import           Control.Lens (Iso', alongside, iso, zoom, (%=), (+=))
import           Control.Lens.TH (makeLenses)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Tree
import           Pos.Util.Chrono
import           Test.QuickCheck

import           Util
import           UTxO.DSL
import           UTxO.Generator
import           Wallet.Inductive

{-------------------------------------------------------------------------------
  Wallet event tree
-------------------------------------------------------------------------------}

-- | Probability (value between 0 and 1)
type Probability = Double

-- | Set of transactions, indexed by their given hash
--
-- We can do this because we will use globally unique hashes
type Transactions h a = Map (GivenHash (Transaction h a)) (Transaction h a)

-- | Parameters for wallet event tree generation
data GenEventsParams h a = GenEventsParams {
      -- | Transaction parameters
      --
      -- NOTE: Right now we use a single set of transaction parameters both
      -- for generating system transactions and for generating wallet
      -- transactions. We may want to split that.
      gepTrParams             :: GenTrParams a

      -- | The set of addresses that belong the wallet
    , gepOurs                 :: Set a

      -- | Initial UTxO
    , gepInitUtxo             :: Utxo h a

      -- | Probability of generating a pending transaction
    , gepPendingProb          :: Probability

      -- | Maximum number of transactions in a block
    , gepMaxBlockSize         :: Int

      -- | Probability that any given transaction in the shared pool will
      -- be included in the next block (see also '_gelsNextBlock')
    , gepSharedCommitProb     :: Probability

      -- | Probability that pending transactions are included in the next block
    , gepPendingCommitProb    :: Probability

      -- | Frequencies of branching factors
      --
      -- For example, @[50, 45, 3, 2]% means that
      --
      -- - Each branch has a probability of 50% of being terminated
      -- - With probability 45% we have a branching factor of 1
      --   (i.e., continue linearly)
      -- - With probability 3% and 2% we branch into 2 and 3 twines.
      --
      -- Note that we terminate a branch only if it is longer than the previous
      -- (so that we never switch to a shorter fork). This means that the first
      -- possiblity (branch termination) may not apply; in that case, the
      -- probabilities of a branching factor of 1, 2 and 3 in this example
      -- become 90%, 6% and 4%, respectively (note that these numbers are
      -- /frequencies/, not probabilities).
    , gepBranchingFrequencies :: [Int]

      -- | Maximum number of forks
      --
      -- This puts a bound on how many forks we create (and tehrefore on the
      -- size of the tree) independent from the branching frequencies
      -- ('gepBranchingFrequencies').
    , gepMaxNumForks          :: Int
    }

defEventsParams :: (Int -> Int -> Value) -- ^ Fee model
                -> [a]             -- ^ Addresses we can generate outputs to
                -> Set a           -- ^ Addresses that belong to the wallet
                -> Utxo h a        -- ^ Initial UTxO
                -> GenEventsParams h a
defEventsParams feeModel
                addresses
                ours
                utxo
                = GenEventsParams {
      gepTrParams             = defTrParams feeModel addresses
    , gepOurs                 = ours
    , gepInitUtxo             = utxo
    , gepPendingProb          = 0.2
    , gepMaxBlockSize         = 10
    , gepSharedCommitProb     = 0.8
    , gepPendingCommitProb    = 0.8
    , gepBranchingFrequencies = [50, 45, 3, 2]
    , gepMaxNumForks          = 5
    }

-- | The global state
--
-- " Global " here refers to " not local to a branch "
data GenEventsGlobalState h a = GenEventsGlobalState {
      -- | We store the hash in the global state so that we generally
      -- globally unique hashes. This makes debugging a bit easier.
      _gegsNextHash  :: Int

      -- | Total set of transactions /ever/ submitted by the wallet
      --
      -- We add new transactions into this set when the wallet, but never
      -- remove from them, and this set is uneffected by rollback
      -- (hence it lives in the global state). This models the fact that
      -- transactions once submitted into the system "stay out there".
    , _gegsPending   :: Transactions h a

      -- | Maximum height of any path through the tree generated so far
    , _gegsMaxLength :: Int

      -- | Number of forks created so far
    , _gegsNumForks  :: Int
    }

-- | Branch local state
data GenEventsLocalState h a = GenEventsLocalState {
      -- | System input state
      --
      -- Used for generating transactions that can use the entire UTxO
      _gelsSystemInpState :: GenInpState h a

      -- | Wallet input state
      --
      -- Used for generating transactions that can only use the wallet's UTxO
    , _gelsWalletInpState :: GenInpState h a

      -- | Transactions we can chose from for the next block we generate
      --
      -- Since we want a high degree of overlap between the sets of transactions
      -- across the twines of a fork, at each branch point we generate a set
      -- of transactions that we can then choose from in the separate branches.
      --
      -- In order to make it possible to do a random selection from this set,
      -- we make sure that these transactions are mutually independent.
    , _gelsNextBlock      :: Transactions h a

       -- | Length of the current branch
    , _gelsLength         :: Int
    }

makeLenses ''GenEventsGlobalState
makeLenses ''GenEventsLocalState

-- | Initial 'GenEventsGlobalState'
initEventsGlobalState :: Int   -- ^ First available hash
                      -> GenEventsGlobalState h a
initEventsGlobalState nextHash = GenEventsGlobalState {
      _gegsNextHash  = nextHash
    , _gegsPending   = Map.empty
    , _gegsMaxLength = 0
    , _gegsNumForks  = 0
    }

-- | Lens to the system UTxO
gelsSystemUtxo :: Lens' (GenEventsLocalState h a) (Utxo h a)
gelsSystemUtxo = gelsSystemInpState . gisUtxo

-- | Lens to the wallet UTxO
gelsWalletUTxO :: Lens' (GenEventsLocalState h a) (Utxo h a)
gelsWalletUTxO = gelsWalletInpState . gisUtxo

-- | Combine local and global state
type GenEventsState h a = (GenEventsLocalState h a, GenEventsGlobalState h a)

-- | Events generator
type GenEvents h a = StateT (GenEventsGlobalState h a) Gen

-- | Branch generator
type GenBranch h a = StateT (GenEventsState h a) Gen

-- | Branch seeds generator
--
-- This is the signature that 'unfoldTreeM' requires.
type GenSeeds h a x = GenEventsLocalState h a -> GenEvents h a (x, [GenEventsLocalState h a])

-- | Lift actions that require the combined state
withCombinedState :: GenEventsLocalState h a
                  -> GenBranch h a x -> GenEvents h a (x, GenEventsLocalState h a)
withCombinedState ls act = StateT $ fmap reassoc . runStateT act . (ls,)
  where
    reassoc :: (a, (b, c)) -> ((a, b), c)
    reassoc (a, (b, c)) = ((a, b), c)

-- | Variation on 'withCombinedState' with no additional return value
withCombinedState_ :: GenEventsLocalState h a
                   -> GenBranch h a () -> GenEvents h a (GenEventsLocalState h a)
withCombinedState_ ls = fmap snd . withCombinedState ls

-- | Split the transaction generation state into the local and global components
splitTrState :: Iso' (GenInpState h a, Int) (GenTrState h a)
splitTrState = iso (uncurry GenTrState) (\(GenTrState x y) -> (x, y))

-- | State for generating system transactions
gesSystemTrState :: Lens' (GenEventsState h a) (GenTrState h a)
gesSystemTrState = (gelsSystemInpState `alongside` gegsNextHash) . splitTrState

-- | State for generating wallet transactions
gesWalletTrState :: Lens' (GenEventsState h a) (GenTrState h a)
gesWalletTrState = (gelsWalletInpState `alongside` gegsNextHash) . splitTrState

-- | Generate event tree
--
-- NOTE: Rollbacks are implicit in the tree structure.
genEventTree :: forall h a. (Hash h a, Ord a)
             => GenEventsParams h a
             -> GenEvents h a (Tree (WalletEvent h a))
genEventTree GenEventsParams{..} =
    unfoldTreeM buildTree initLocalState
  where
    buildTree :: GenSeeds h a (WalletEvent h a)
    buildTree ls = do
        shouldSubmitPending <- lift $ toss gepPendingProb
        if shouldSubmitPending
          then submitPending ls
          else generateBlock ls

    -- Try to submit a new pending transaction
    --
    -- If this fails (if the wallet has no inputs available), we just repeat
    -- the random choice in 'buildTree'.
    submitPending :: GenSeeds h a (WalletEvent h a)
    submitPending ls = do
        pending <- use gegsPending
        (mTr, ls') <- withCombinedState ls $ zoom gesWalletTrState $
           -- Pending transactions don't affect the UTxO, but should not use
           -- inputs already used by other pending transactions
           genTransaction
             gepTrParams
             DontRemoveUsedInputs
             DontMakeOutputsAvailable
             (Set.unions $ map trIns $ Map.elems pending)
        case mTr of
          Nothing -> buildTree ls'
          Just tr -> do gegsPending %= Map.insert (givenHash tr) tr
                        return (NewPending tr, [ls'])

    -- Generate a block
    generateBlock :: GenSeeds h a (WalletEvent h a)
    generateBlock ls = do
        -- Create the block itself
        (ev, ls') <- withCombinedState ls $ do
           blockSize <- lift $ choose (0, gepMaxBlockSize)

           -- First, we choose some pending transactions to commit
           availablePending <- Map.toList <$> use (_2 . gegsPending)
           (_pendingHashes, committedPending) <-
             unzip <$> commitSome gepPendingCommitProb
                                  (take blockSize availablePending)
           let remaining = blockSize - length committedPending

           -- Next, we choose some transactions from the shared pool
           availableShared <- Map.toList <$> use (_1 . gelsNextBlock)
           (sharedHashes, committedShared) <-
             unzip <$> commitSome gepSharedCommitProb
                                  (take remaining availableShared)
           (_1 . gelsNextBlock) %= (`withoutKeys` Set.fromList sharedHashes)
           let remaining' = remaining - length committedShared

           -- Finally, we create some transactions specific to this block
           --
           -- For these final transactions, we make the outputs of the
           -- already selected transactions available, so that the block does
           -- not only consist of independent transactions. This means we now
           -- also need to pick an ordering.
           alreadyCommitted <- lift $ shuffle $ committedPending ++ committedShared
           let newUtxo = utxoUnions $ map trUtxo alreadyCommitted
           (_1 . gelsSystemUtxo) %= utxoUnion newUtxo
           committedRest <- zoom gesSystemTrState $
               replicateAtMostM remaining' $
                 genTransaction
                   gepTrParams
                   RemoveUsedInputs
                   MakeOutputsAvailable
                   Set.empty

           -- Increase length of current branch
           (_1 . gelsLength) += 1

           -- Apply the block to the wallet's UTxO
           let block = OldestFirst (alreadyCommitted ++ committedRest)
           (_1 . gelsWalletUTxO) %= updateWalletUtxo block

           return $ ApplyBlock block

        -- Generate some transactions to be included in the next block
        ls'' <- withCombinedState_ ls' $ do
            stillAvailable <- Map.size <$> use (_1 . gelsNextBlock)
            newAvailable   <- zoom gesSystemTrState $
                replicateAtMostM (gepMaxBlockSize - stillAvailable) $ do
                  genTransaction
                    gepTrParams
                    RemoveUsedInputs
                    DontMakeOutputsAvailable
                    Set.empty
            let newAvailable' = map (\tr -> (givenHash tr, tr)) newAvailable
            _1 . gelsNextBlock %= Map.union (Map.fromList newAvailable')

        -- Finally, decide how to branch
        --
        -- A branching factor of 0 means that we terminate this branch at this
        -- point, a branching factor of 1 is just a linear continuation, and
        -- a higher branching factor is a proper fork.
        --
        -- We can terminate this branch at this point only if we have exceeded
        -- the maximum height seen so far, guaranteeing that each next
        -- path through the tree is longer than the previous. This is necessary,
        -- because we only ever switch to a fork when the new fork is longer
        -- than the current
        let ourLength = ls'' ^. gelsLength
        maxLength <- use gegsMaxLength
        numForks  <- use gegsNumForks
        let allowedTerminate, allowedFork :: Bool
            allowedTerminate = ourLength > maxLength
            allowedFork      = numForks < gepMaxNumForks

            -- Is a particular branching factor applicable?
            applicable :: (Int, Int) -> Bool
            applicable (_freq, 0) = allowedTerminate
            applicable (_freq, 1) = True
            applicable (_freq, _) = allowedFork

            -- Applicable branching frequencies
            freqs :: [(Int, Int)]
            freqs = filter applicable $ zip gepBranchingFrequencies [0 ..]

        branchingFactor <- lift $ frequency $ map (second pure) freqs

        gegsMaxLength %= max ourLength
        gegsNumForks  += if branchingFactor > 1 then 1 else 0

        return (ev, replicate branchingFactor ls'')

    -- Commit some of the given transactions
    commitSome :: Probability
               -> [(GivenHash (Transaction h a), Transaction h a)]
               -> GenBranch h a [(GivenHash (Transaction h a), Transaction h a)]
    commitSome p trs = do
        fmap catMaybes <$> forM trs $ \(h, tr) -> do
          shouldCommit <- lift $ toss p
          canCommit    <- (`checkCanCommit` tr) <$> use (_1 . gelsSystemUtxo)
          if not (shouldCommit && canCommit)
            then return Nothing
            else do (_1 . gelsSystemUtxo) %= utxoRemoveInputs (trIns tr)
                    return $ Just (h, tr)

    -- Check if all inputs are available in the given UTxO
    checkCanCommit :: Utxo h a -> Transaction h a -> Bool
    checkCanCommit u = all (`Set.member` utxoDomain u) . Set.toList . trIns

    -- Update the wallet's UTxO
    updateWalletUtxo :: Block h a -> Utxo h a -> Utxo h a
    updateWalletUtxo b = utxoRestrictToAddr ours . utxoApplyBlock b

    -- Addresses owned by the wallet
    ours :: a -> Bool
    ours = (`Set.member` gepOurs)

    -- Initial local state at the root of the tree
    initLocalState :: GenEventsLocalState h a
    initLocalState = GenEventsLocalState {
          _gelsSystemInpState = initInpState initSystemUtxo
        , _gelsWalletInpState = initInpState initWalletUtxo
        , _gelsNextBlock      = Map.empty
        , _gelsLength         = 0
        }
      where
        initSystemUtxo = gepInitUtxo
        initWalletUtxo = utxoRestrictToAddr ours gepInitUtxo

{-------------------------------------------------------------------------------
  Generate wallet events
-------------------------------------------------------------------------------}

genWalletEvents :: forall h a. (Hash h a, Ord a)
                => GenEventsParams h a
                -> GenEvents h a (OldestFirst [] (WalletEvent h a))
genWalletEvents = fmap linearise . genEventTree

-- | Linearise a tree of events to a list of events
linearise :: Tree (WalletEvent h a) -> OldestFirst [] (WalletEvent h a)
linearise = OldestFirst . stripRollbacks . go
  where
    -- Preorder traversal, matching each @ApplyBlock@ with a @Rollback@.
    go :: Tree (WalletEvent h a) -> [WalletEvent h a]
    go (Node (NewPending t) branches) =
        -- We never split on 'NewPending'
        let [branch] = branches in NewPending t : go branch
    go (Node (ApplyBlock b) branches) = concat [
          [ApplyBlock b]
        , concatMap go branches
        , [Rollback]
        ]
    go (Node Rollback _branches) =
        error "linearise: unexpected Rollback"

    -- each time we go back up the tree, we generate rollbacks; but we don't
    -- want to do that for the very last branch.
    stripRollbacks :: [WalletEvent h a] -> [WalletEvent h a]
    stripRollbacks = reverse . dropWhile walletEventIsRollback . reverse

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Weighted coin toss
--
-- @toss p@ throws a p-weighted coin and returns whether it came up heads.
-- @toss 0@ will always return @False@, @toss 1@ will always return @True@.
toss :: Probability -> Gen Bool
toss 0 = return False
toss 1 = return True
toss p = (< p) <$> choose (0, 1)
