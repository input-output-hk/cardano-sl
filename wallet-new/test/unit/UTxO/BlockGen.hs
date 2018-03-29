{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

module UTxO.BlockGen
    ( genValidBlockchain
    , genValidBlocktree
    , divvyUp
    , selectDestination
    , selectDestinations'
    , estimateFee
    ) where

import           Universum hiding (use, (%~), (.~), (^.))

import           Control.Category (id)
import           Control.Lens hiding (elements)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Tree (Tree)
import qualified Data.Tree as Tree
import           Pos.Util.Chrono
import           Test.QuickCheck

import           Util.DepIndep
import           UTxO.Context
import           UTxO.DSL
import           UTxO.PreChain

-- | Blockchain Generator Monad
--
-- When generating transactions, we need to keep track of addresses,
-- balances, and transaction IDs so that we can create valid future
-- transactions without wasting time searching and validating.
newtype BlockGen h a
    = BlockGen
    { unBlockGen :: StateT (BlockGenCtx h) Gen a
    } deriving (Functor, Applicative, Monad, MonadState (BlockGenCtx h))

-- | The context and settings for generating arbitrary blockchains.
data BlockGenCtx h
    = BlockGenCtx
    { _blockGenCtxCurrentUtxo            :: !(Utxo h Addr)
    -- ^ The mapping of current addresses and their current account values.
    , _blockGenCtxFreshHashSrc           :: !Int
    -- ^ A fresh hash value for each new transaction.
    , _blockGenCtxInputPartiesUpperLimit :: !Int
    -- ^ The upper limit on the number of parties that may be selected as
    -- inputs to a transaction
    }

makeFields ''BlockGenCtx

genValidBlockchain :: Hash h Addr => PreChain h Gen ()
genValidBlockchain = toPreChain newChain

toPreChain
    :: Hash h Addr
    => BlockGen h [[Value -> Transaction h Addr]]
    -> PreChain h Gen ()
toPreChain = toPreChainWith identity

toPreChainWith
    :: Hash h Addr
    => (BlockGenCtx h -> BlockGenCtx h)
    -> BlockGen h [[Value -> Transaction h Addr]]
    -> PreChain h Gen ()
toPreChainWith settings bg = DepIndep $ \boot -> do
    ks <- runBlockGenWith settings boot bg
    return $ \fees -> (markOldestFirst (zipFees ks fees), ())
  where
   markOldestFirst = OldestFirst . fmap OldestFirst

-- | Given an initial bootstrap 'Transaction' and a function to customize
-- the other settings in the 'BlockGenCtx', this function will initialize
-- the generator and run the action provided.
runBlockGenWith
    :: Hash h Addr
    => (BlockGenCtx h -> BlockGenCtx h)
    -> Transaction h Addr
    -> BlockGen h a
    -> Gen a
runBlockGenWith settings boot m =
    evalStateT (unBlockGen m) (settings (initializeCtx boot))

-- | Create an initial context from the boot transaction.
initializeCtx :: Hash h Addr => Transaction h Addr -> BlockGenCtx h
initializeCtx boot = BlockGenCtx {..}
  where
    _blockGenCtxCurrentUtxo = trUtxo boot
    _blockGenCtxFreshHashSrc = 1
    _blockGenCtxInputPartiesUpperLimit = 1

-- | Lift a 'Gen' action into the 'BlockGen' monad.
liftGen :: Gen a -> BlockGen h a
liftGen = BlockGen . lift

-- | Provide a fresh hash value for a transaction.
freshHash :: ( HasFreshHashSrc src Int
             , MonadState src m
             )
          => m Int
freshHash = do
    i <- use freshHashSrc
    freshHashSrc += 1
    pure i

nonAvvmUtxo :: HasCurrentUtxo src (Utxo h Addr)
            => Getter src (Utxo h Addr)
nonAvvmUtxo =
    currentUtxo . to (utxoRestrictToAddr (not . isAvvmAddr))

selectSomeInputs' :: Hash h Addr
                  => Int
                  -> Utxo h Addr
                  -> Gen (NonEmpty (Input h Addr, Output Addr))
selectSomeInputs' upperLimit (utxoToMap -> utxoMap) = do
    input1 <- elements (Map.toList utxoMap)
    -- it seems likely that we'll want to weight the frequency of
    -- just-one-input more heavily than lots-of-inputs
    n <- frequency $ zip
            [upperLimit, upperLimit-1 .. 0]
            (map pure [0 .. upperLimit])
    otherInputs <- loop (Map.delete (fst input1) utxoMap) n
    pure (input1 :| otherInputs)
  where
    loop utxo n
        | n <= 0 = pure []
        | otherwise = do
            inp <- elements (Map.toList utxo)
            rest <- loop (Map.delete (fst inp) utxo) (n - 1)
            pure (inp : rest)

selectSomeInputs :: Hash h Addr => BlockGen h (NonEmpty (Input h Addr, Output Addr))
selectSomeInputs = do
    utxoMap <- use nonAvvmUtxo
    upperLimit <- use inputPartiesUpperLimit
    liftGen $ selectSomeInputs' upperLimit utxoMap

selectDestinations :: Hash h Addr => Set (Input h Addr) -> BlockGen h (NonEmpty Addr)
selectDestinations notThese =
    liftGen . selectDestinations' notThese =<< use currentUtxo

-- | FIXME: This returns just one 'Addr' wrapped in a 'NonEmpty', drop the thing
-- and use 'selectDestination' instead.
selectDestinations'
    :: Hash h Addr
    => Set (Input h Addr)
    -> Utxo h Addr
    -> Gen (NonEmpty Addr)
selectDestinations' notThese u0 =
  pure <$> selectDestination (utxoRemoveInputs notThese u0)

selectDestination :: Hash h Addr => Utxo h Addr -> Gen Addr
selectDestination u0 = do
  -- AVVM addresses can't ever receive deposits, so we exclude them.
  let u1 = utxoRestrictToAddr (not . isAvvmAddr) u0
  elements (map (outAddr . snd) (utxoToList u1))


-- | Create a fresh transaction that depends on the fee provided to it.
newTransaction :: (HasCallStack, Hash h Addr)
               => BlockGen h (Value -> Transaction h Addr)
newTransaction = do
    inputs'outputs <- selectSomeInputs
    destinations <- selectDestinations (foldMap Set.singleton (map fst inputs'outputs))
    hash' <- freshHash

    let txn = divvyUp hash' inputs'outputs destinations

    -- We don't know the fee yet, but /do/ need to make it possible to
    -- generate different kinds of transactions (i.e., different kinds of
    -- monadic effects) depending on the UTxO. This means that we must be
    -- conversative here.
    currentUtxo %= utxoApply (withEstimatedFee txn)
    pure txn

-- | Given a set of inputs, tagged with their output values, and a set of output
-- addresses, construct a transaction by dividing the sum total of the inputs
-- evenly over the output addresses.
divvyUp
    :: (HasCallStack, Hash h Addr)
    => Int -- ^ hash
    -> NonEmpty (Input h Addr, Output Addr)
    -> NonEmpty Addr
    -> Value
    -> Transaction h Addr
divvyUp h inputs'outputs destinations fee = tx
  where
    tx = Transaction {
             trFresh = 0
           , trFee   = fee
           , trHash  = h
           , trIns   = inputs
           , trOuts  = outputs
           , trExtra = []
           }
    inputs = foldMap (Set.singleton . fst) inputs'outputs
    destLen = fromIntegral (length destinations)
    -- if we don't know what the fee is yet (eg a 0), then we want to use
    -- the max fee for safety's sake
    totalValue = sum (map (outVal . snd) inputs'outputs)
        `safeSubtract` if fee == 0 then estimateFee tx else fee
    valPerOutput = totalValue `div` destLen
    outputs = toList (map (\addr -> Output addr valPerOutput) destinations)

-- | 'Value' is an alias for 'Word64', which underflows. This detects
-- underflow and returns @0@ for underflowing values.
safeSubtract :: Value -> Value -> Value
safeSubtract x y
    | z > x     = 0
    | otherwise = z
  where
    z = x - y

-- | Conversatively estimate the fee for this transaction
--
-- Result may be larger than the minimum fee, but not smaller.
-- TODO: Right now this does not take the transaction structure into account.
-- We should come up with a more precise model here.
estimateFee :: Transaction h a -> Value
estimateFee _ = maxFee
  where
    maxFee = 180000

withEstimatedFee :: (Value -> Transaction h a) -> Transaction h a
withEstimatedFee tx = let tx0 = tx 0 in tx0 { trFee = estimateFee tx0 }

newBlock :: Hash h Addr => BlockGen h [Value -> Transaction h Addr]
newBlock = do
    txnCount <- liftGen $ choose (1, 10)
    replicateM txnCount newTransaction

newChain :: Hash h Addr => BlockGen h [[Value -> Transaction h Addr]]
newChain = do
    blockCount <- liftGen $ choose (10, 50)
    replicateM blockCount newBlock

zipFees
    :: [[Value -> Transaction h Addr]]
    -> ([[Value]] -> [[Transaction h Addr]])
zipFees = zipWith (zipWith ($))

{-------------------------------------------------------------------------------
  Forkable blockchains
-------------------------------------------------------------------------------}

type NoFeeBlockTree h = Tree [Value -> Transaction h Addr]

-- | Global context for building block trees.
--   This structure is shared between all branches, and controls the
--   extent to which the tree forks.
data TreeGenGlobalCtx h = TreeGenGlobalCtx
    { _treeGenGlobalCtxFreshHashSrc                :: !Int
      -- ^ A fresh hash value for each new transaction.
    {- The below values form part of configuration, and are unlikely to be
       updated during tree building.
    -}
    , _treeGenGlobalCtxInputPartiesUpperLimit      :: !Int
      -- ^ The upper limit on the number of parties that may be selected as
      -- inputs to a transaction.
    , _treeGenGlobalCtxForkLikelihood              :: !Double
      -- ^ Likelihood of generating a forked block at any point in the tree.
      --   Only the non-integer part of this value is considered.
    , _treeGenGlobalCtxPruneLikelihood             :: !Double
      -- ^ Likelihood of the non-principal branch to be terminated at any
      --   block.
    , _treeGenGlobalCtxMaxHeight                   :: !Int
      -- ^ Maximum height of the tree. The principal branch will be grown to
      --   this height. In order to preserve the property that we only switch
      --   to longer forks, if any other branch grows to this height, it will
      --   be terminated at (maxHeight - 1).
    , _treeGenGlobalCtxSharedTransactionLikelihood :: !Double
      -- ^ The likelihood that a given transaction will be shared between two
      --   blocks with the same parent.
    , _treeGenGlobalCtxBootTransaction             :: Transaction h Addr
      -- ^ Boot transaction
    }

makeFields ''TreeGenGlobalCtx

initTreeGenGlobalCtx :: Transaction h Addr -> TreeGenGlobalCtx h
initTreeGenGlobalCtx boot = TreeGenGlobalCtx
  { _treeGenGlobalCtxFreshHashSrc = 1
  , _treeGenGlobalCtxInputPartiesUpperLimit = 1
  , _treeGenGlobalCtxForkLikelihood = 0.1
  , _treeGenGlobalCtxPruneLikelihood = 0.3
  , _treeGenGlobalCtxMaxHeight = 50
  , _treeGenGlobalCtxSharedTransactionLikelihood = 0.5
  , _treeGenGlobalCtxBootTransaction = boot
  }

data TreeGenBranchCtx h = TreeGenBranchCtx
    { _treeGenBranchCtxPrincipalBranch :: !Bool
      -- ^ Is this the principal branch? The principal branch will never
      --   be pruned.
    , _treeGenBranchCtxBranchHeight    :: !Int
      -- ^ Height of the tip of the branch (from the root).
    , _treeGenBranchCtxCurrentUtxo     :: !(Utxo h Addr)
      -- ^ The mapping of current addresses and their current account values.
    , _treeGenBranchCtxTransactions    :: ![Value -> Transaction h Addr]
      -- ^ Transactions to form this block. This is stored here because we
      --   generate all transactions for the child block at the parent level,
      --   in order to facilitate sharing.
    }

makeFields ''TreeGenBranchCtx

-- | Tree generation monad.
newtype TreeGen h a = TreeGen
    { unTreeGen :: StateT (TreeGenGlobalCtx h) Gen a
    } deriving (Functor, Applicative, Monad, MonadState (TreeGenGlobalCtx h))

-- | Lift a 'Gen' action into the 'TreeGen' monad.
liftGenTree :: Gen a -> TreeGen h a
liftGenTree = TreeGen . lift

genValidBlocktree :: Hash h Addr => PreTree h Gen ()
genValidBlocktree = toPreTreeWith identity newTree

toPreTreeWith
    :: Hash h Addr
    => (TreeGenGlobalCtx h -> TreeGenGlobalCtx h) -- ^ Modify the global settings
    -> TreeGen h (NoFeeBlockTree h)
    -> PreTree h Gen ()
toPreTreeWith settings bg = DepIndep $ \(boot :: Transaction h Addr)-> do
    ks <- evalStateT (unTreeGen bg) (settings (initTreeGenGlobalCtx boot))
    return $ \fees ->
      (OldestFirst (fmap (OldestFirst . dropWhile (== boot))
                         (zipTreeFees ks fees)), ())

newTree :: forall h. Hash h Addr
        => TreeGen h (NoFeeBlockTree h)
newTree = do
    boot <- use bootTransaction
    -- Choose a random height for the blocktree
    height <- liftGenTree $ choose (10, 50)
    maxHeight .= height
    Tree.unfoldTreeM buildTree $ initBranchCtx boot
  where
    initBranchCtx boot = TreeGenBranchCtx True 0 (trUtxo boot) [const boot]
    buildTree :: TreeGenBranchCtx h
              -> TreeGen h ([Value -> Transaction h Addr], [TreeGenBranchCtx h])
    buildTree branchCtx = do
      let curHeight = branchCtx ^. branchHeight

      -- Firstly, decide whether we should prune this branch. We prune if
      -- - we have reached the maximum height, or
      -- - we are not the principal branch, and
      -- - - we have reached the maximum height - 1, or
      -- - - with probability equal to the prune likelihood
      toPrune <- do
        pl <- use pruneLikelihood
        maxH <- use maxHeight
        toss <- liftGenTree $ choose (0,1)
        return $ (curHeight >= maxH)
               || ((not $ branchCtx ^. principalBranch)
                   && ((curHeight >= maxH - 1) || toss < pl)
                  )

      if toPrune
      then return (branchCtx ^. transactions, [])
      else do
        -- At each 'level', we first work out how many branches to generate. We
        -- then generate a number of transactions T and select from the set of
        -- transactions (with replacement). This should result in a relatively
        -- high degree of transactions shared between branches.
        numBranches <- case curHeight of
          0 -> pure 1  -- we don't branch on the first block.
          _ -> liftGenTree . branchCount =<< use forkLikelihood
        branchSizes <- liftGenTree $ vectorOf numBranches $ choose (1, 10)
        stl <- use sharedTransactionLikelihood
        txs <- replicateM
          (ceiling $ fromIntegral (maximum branchSizes) / stl)
          (generateTransaction branchCtx)

        -- One may ask why we set the 'last' branch as principal. This is to
        -- reduce the amount of state being carried during constructing. The
        -- principal branch is expected to be the longest, so we would like to
        -- finish constructing short forks early.
        branches <- forM (zip [0..] branchSizes) $ \(idx, bs) -> do
          mytxs <- liftGenTree $ replicateM bs $ elements txs
          return $ branchCtx
            & (branchHeight +~ 1)
            . (principalBranch .~ (idx == numBranches -1))
            . (transactions .~ txs)
            . (currentUtxo %~ (foldr (.) id $ utxoApply . withEstimatedFee <$> mytxs))


        return (branchCtx ^. transactions, branches)
    -- Calculate the number of branches to generate. We keep tossing a
    -- 'p'-weighted coin until we get a tails.
    branchCount :: Double -> Gen Int
    branchCount p = go 1 where
      go count = do
        toss <- choose (0,1)
        if toss > p then return count else go $ count + 1
    generateTransaction branchCtx = do
      hash' <- freshHash
      limit <- use inputPartiesUpperLimit
      inputs'outputs <- liftGenTree
        $ selectSomeInputs' limit (branchCtx ^. currentUtxo)
      destinations <- liftGenTree
        $ selectDestinations' (foldMap Set.singleton (map fst inputs'outputs))
                              (branchCtx ^. currentUtxo)
      return $ divvyUp hash' inputs'outputs destinations

zipTreeFees
    :: Tree [Value -> Transaction h Addr]
    -> (Tree [Value] -> Tree [Transaction h Addr])
zipTreeFees = curry $ Tree.unfoldTree go
  where
    go (Tree.Node f txChildren, Tree.Node fee feeChildren) =
      (zipWith ($) f fee, zip txChildren feeChildren)
