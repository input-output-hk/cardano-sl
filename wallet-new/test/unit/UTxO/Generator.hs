{-# LANGUAGE TemplateHaskell #-}

module UTxO.Generator (
    -- * Inputs
    -- ** State
    GenInpState(..)
  , gisUtxo
  , initInpState
    -- ** Generator
  , RemoveUsedInputs(..)
  , GenInput
  , genInput
    -- * Outputs
    -- ** Parameters
  , GenOutParams(..)
  , defOutParams
    -- ** State
  , GenOutState(..)
  , gosAvailable
  , initOutState
    -- ** Generator
  , GenOutput
  , genOutput
    -- * Transactions
    -- ** Parameters
  , GenTrParams(..)
  , defTrParams
    -- ** State
  , GenTrState(..)
  , gtsInpState
  , gtsNextHash
  , initTrState
    -- ** Generator
  , GenTransaction
  , MakeOutputsAvailable(..)
  , genTransaction
    -- * Chains
    -- ** Params
  , GenChainParams(..)
  , defChainParams
    -- ** Generator
  , genChain
    -- * Auxiliary
  , replicateAtMostM
  ) where

import           Universum

import           Control.Lens (zoom, (%=), (.=), (<<+=))
import           Control.Lens.TH (makeLenses)
import qualified Data.Set as Set
import           Pos.Core (maxCoinVal)
import           Pos.Util.Chrono
import           Test.QuickCheck

import           UTxO.DSL

{-------------------------------------------------------------------------------
  Generate transaction inputs
-------------------------------------------------------------------------------}

-- | State needed for input generation
data GenInpState h a = GenInpState {
      -- | Available UTxO
      _gisUtxo :: Utxo h a
    }

makeLenses ''GenInpState

-- | Initial 'GenInpState'
initInpState :: Utxo h a -> GenInpState h a
initInpState utxo = GenInpState {
      _gisUtxo = utxo
    }

-- | Input generator
type GenInput h a = StateT (GenInpState h a) Gen

-- | Should we remove used inputs?
data RemoveUsedInputs = RemoveUsedInputs | DontRemoveUsedInputs

-- | Try to generate an input
--
-- Returns nothing if the utxo is empty.
genInput :: Hash h a
         => RemoveUsedInputs
         -> Set (Input h a) -- Inputs to avoid
         -> GenInput h a (Maybe (Input h a, Output a))
genInput removeUsedInputs notThese = do
    utxo <- utxoRemoveInputs notThese <$> use gisUtxo
    if utxoNull utxo
      then return Nothing
      else do
        (inp, out) <- lift $ elements (utxoToList utxo)

        case removeUsedInputs of
          DontRemoveUsedInputs -> return ()
          RemoveUsedInputs     -> gisUtxo .= utxoRemoveInputs (Set.singleton inp) utxo

        return $ Just (inp, out)

-- | Generate up to @n@ distinct inputs
genDistinctInputs :: forall h a. Hash h a
                  => RemoveUsedInputs
                  -> Set (Input h a) -- Inputs to avoid
                  -> Int
                  -> GenInput h a [(Input h a, Output a)]
genDistinctInputs removeUsedInputs = go
  where
    go :: Set (Input h a) -> Int -> GenInput h a [(Input h a, Output a)]
    go _        0 = return []
    go notThese n = do
        mInp <- genInput removeUsedInputs notThese
        case mInp of
          Nothing ->
            return []
          Just (inp, out) ->
            -- Removing used inputs or not, don't select same input twice
            ((inp, out) :) <$> go (Set.insert inp notThese) (n - 1)

{-------------------------------------------------------------------------------
  Generate transaction outputs
-------------------------------------------------------------------------------}

-- | Parameters for output generation
data GenOutParams a = GenOutParams {
      -- | Addresses we can generate outputs to
      gopAddresses :: [a]
    }

-- | Default 'GenOutParams'
defOutParams :: [a] -- ^ Addresses we can generate outputs to
             -> GenOutParams a
defOutParams addresses = GenOutParams {
      gopAddresses = addresses
    }

-- | State needed for output generation
data GenOutState = GenOutState {
      -- | Value left
      _gosAvailable :: Value
    }

makeLenses ''GenOutState

-- | Initial 'GenOutState'
initOutState :: Value -> GenOutState
initOutState available = GenOutState {
      _gosAvailable = available
    }

-- | Output generator
type GenOutput = StateT GenOutState Gen

-- | Try to generate transaction output
--
-- Returns nothing if there is no balance left.
genOutput :: GenOutParams a -> GenOutput (Maybe (Output a))
genOutput GenOutParams{..} = do
    available <- use gosAvailable
    if available == 0
      then return Nothing
      else do
        addr <- lift $ elements gopAddresses
        val  <- lift $ choose (1, min available maxCoinVal)
        gosAvailable .= available - val
        return $ Just (Output addr val)

{-------------------------------------------------------------------------------
  Generate transaction
-------------------------------------------------------------------------------}

-- | Parameters for transaction generation
data GenTrParams a = GenTrParams {
      -- | Maximum number of inputs
      --
      -- Generation is biased towards smaller values
      gtpMaxNumInputs  :: Int

      -- | Maximum number of outputs
      --
      -- Generation does a uniform draw.
    , gtpMaxNumOutputs :: Int

      -- | Fee model
      --
      -- Provide fee given number of inputs and outputs
    , gtpEstimateFee   :: Int -> Int -> Value

      -- | Output parameters
    , gtpOutParams     :: GenOutParams a
    }

-- | Default 'GenTrParams'
defTrParams :: (Int -> Int -> Value) -- ^ Fee model
            -> [a]                   -- ^ Addresses we can generate outputs to
            -> GenTrParams a
defTrParams feeModel addresses = GenTrParams {
      gtpMaxNumInputs  = 3
    , gtpMaxNumOutputs = 3
    , gtpEstimateFee   = feeModel
    , gtpOutParams     = defOutParams addresses
    }

-- | Should the outputs of the transaction be made available in the
-- input generation state?
data MakeOutputsAvailable =
    -- | Yes, make outputs available
    --
    -- This means that the next generation can refer to outputs of the
    -- previous generated transaction.
    MakeOutputsAvailable

    -- | No, don't make output available
    --
    -- Use to generate independent transactions.
  | DontMakeOutputsAvailable

-- | State needed for transaction generation
data GenTrState h a = GenTrState {
      -- | State needed to generate inputs
      _gtsInpState :: GenInpState h a

      -- | Next hash
    , _gtsNextHash :: Int
    }

makeLenses ''GenTrState

-- | Initial 'GenTrState'
initTrState :: Utxo h a       -- ^ Initial UTxO
            -> Int            -- ^ First available hash
            -> GenTrState h a
initTrState utxo nextHash = GenTrState {
      _gtsInpState = initInpState utxo
    , _gtsNextHash = nextHash
    }

-- | Transaction generator
type GenTransaction h a = StateT (GenTrState h a) Gen

-- | Try to generate a transaction
--
-- Fails if no inputs were available.
genTransaction :: Hash h a
               => GenTrParams a
               -> RemoveUsedInputs
               -> MakeOutputsAvailable
               -> Set (Input h a)       -- ^ Inputs to avoid
               -> GenTransaction h a (Maybe (Transaction h a))
genTransaction GenTrParams{..} removeUsedInputs makeOutputsAvailable notThese = do
    numInputs <- lift $ chooseNumInputs
    inputs    <- zoom gtsInpState $
                   genDistinctInputs removeUsedInputs notThese numInputs

    if null inputs
      then return Nothing
      else do
        nextHash   <- gtsNextHash <<+= 1
        numOutputs <- lift $ choose (1, gtpMaxNumOutputs)

        let fee     = gtpEstimateFee numInputs numOutputs
            inValue = sum (map (outVal . snd) inputs)

        if inValue <= fee
          then return Nothing
          else do
            let gos = initOutState (inValue - fee)

            outputs <- lift $ (`evalStateT` gos) $
                         replicateAtMostM numOutputs $ genOutput gtpOutParams

            let tr = Transaction {
                         trFresh = 0
                       , trIns   = Set.fromList (map fst inputs)
                       , trOuts  = outputs
                       , trFee   = fee
                       , trHash  = nextHash
                       , trExtra = []
                       }

            case makeOutputsAvailable of
              DontMakeOutputsAvailable -> return ()
              MakeOutputsAvailable     -> zoom gtsInpState $
                                            (gisUtxo %= utxoUnion (trUtxo tr))

            return $ Just tr
  where
    -- Bias towards fewer inputs
    chooseNumInputs :: Gen Int
    chooseNumInputs = frequency $ zip [gtpMaxNumInputs, gtpMaxNumInputs-1 ..]
                                      (map pure [1 .. gtpMaxNumInputs])

{-------------------------------------------------------------------------------
  Chains
-------------------------------------------------------------------------------}

data GenChainParams a = GenChainParams {
      -- | Maximum number of transactions per block
      gcpMaxBlockSize   :: Int

      -- | Maximum number of blocks
    , gcpMaxChainLength :: Int

      -- | Transaction parameters
    , gcpTrParams       :: GenTrParams a
    }

-- | Default 'GenChainParams'
defChainParams :: (Int -> Int -> Value) -- ^ Fee model
               -> [a]                   -- ^ Address we can generate outputs for
               -> GenChainParams a
defChainParams feeModel addresses = GenChainParams {
      gcpMaxBlockSize   = 20
    , gcpMaxChainLength = 10
    , gcpTrParams       = defTrParams feeModel addresses
    }

-- | Generate an arbitrary chain
--
-- The chain will have at least one block, but blocks may be empty.
genChain :: forall h a. Hash h a
         => GenChainParams a -> GenTransaction h a (Chain h a)
genChain GenChainParams{..} = goChain
  where
    goChain :: GenTransaction h a (Chain h a)
    goChain = OldestFirst <$> do
        chainLength <- lift $ choose (1, gcpMaxChainLength)
        replicateM chainLength goBlock

    goBlock :: GenTransaction h a (Block h a)
    goBlock = OldestFirst <$> do
        blockSize <- lift $ choose (0, gcpMaxBlockSize)
        replicateAtMostM blockSize $
          genTransaction gcpTrParams RemoveUsedInputs MakeOutputsAvailable Set.empty

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

replicateAtMostM :: Monad m => Int -> m (Maybe a) -> m [a]
replicateAtMostM 0 _ = return []
replicateAtMostM n f = do
    ma <- f
    case ma of
      Just a  -> (a :) <$> replicateAtMostM (n - 1) f
      Nothing -> return []
