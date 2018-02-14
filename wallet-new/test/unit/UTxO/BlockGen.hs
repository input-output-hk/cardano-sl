{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module UTxO.BlockGen where

import           Universum hiding (use, (.~))

import           Control.Lens hiding (elements)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Traversable (for)
import           Pos.Util.Chrono (OldestFirst (..))
import           Test.QuickCheck

import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.DSL
import           UTxO.Interpreter
import           UTxO.PreChain
import           UTxO.Translate

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
    { _bgcAddressesToBalances :: !(Map Addr Value)
    -- ^ The mapping of current addresses and their current account values.
    , _bgcFreshHash           :: !Int
    -- ^ A fresh hash value for each new transaction.
    , _bgcCurrentBlockchain   :: !(Ledger h Addr)
    }

makeLenses ''BlockGenCtx

toPreChain
    :: Hash h Addr
    => BlockGen h [[Value -> Transaction h Addr]]
    -> PreChain h Gen
toPreChain = toPreChainWith identity

toPreChainWith
    :: Hash h Addr
    => (BlockGenCtx h -> BlockGenCtx h)
    -> BlockGen h [[Value -> Transaction h Addr]]
    -> PreChain h Gen
toPreChainWith settings bg = PreChain $ \boot -> do
    ks <- runBlockGenWith settings boot bg
    return
        $ OldestFirst . reverse
        . fmap (OldestFirst . reverse)
        . zipFees ks

-- | Given an initial bootstrap 'Transaction', this function will
-- initialize the generator and run the action provided.
runBlockGen
    :: Hash h Addr
    => Transaction h Addr
    -> BlockGen h a
    -> Gen a
runBlockGen = runBlockGenWith identity

-- | Given an initial bootstrap 'Transaction' and a function to customize
-- the other settings in the 'BlockGenCtx', this function will initialize
-- the generator and run the action provided.
runBlockGenWith
    :: Hash h Addr
    => (BlockGenCtx h -> BlockGenCtx h)
    -> Transaction h Addr
    -> BlockGen h a
    -> Gen a
runBlockGenWith settings boot (BlockGen m) =
    evalStateT m (settings (initializeCtx boot))

-- | Create an initial context from the boot transaction.
initializeCtx :: Hash h Addr => Transaction h Addr -> BlockGenCtx h
initializeCtx boot@Transaction{..} = BlockGenCtx {..}
  where
    _bgcAddressesToBalances =
        foldl' outToValue mempty trOuts
    outToValue acc (Output {..}) =
        Map.insertWith (+) outAddr outVal acc
    _bgcFreshHash = 0
    _bgcCurrentBlockchain = ledgerSingleton boot

-- | Lift a 'Gen' action into the 'BlockGen' monad.
liftGen :: Gen a -> BlockGen h a
liftGen = BlockGen . lift

-- | Provide a fresh hash value for a transaction.
freshHash :: BlockGen h Int
freshHash = do
    i <- use bgcFreshHash
    bgcFreshHash += 1
    pure i

-- | Select a random address from the current state.
selectFromAddr :: BlockGen h Addr
selectFromAddr =
    liftGen . elements =<< uses bgcAddressesToBalances (toList . Map.keysSet)

-- | Select a random address from the current state that is not the
-- provided address.
selectToAddress :: Addr -> BlockGen h Addr
selectToAddress addr = do
    addrs <- uses bgcAddressesToBalances (toList . Set.delete addr . Map.keysSet)
    liftGen (elements (filter (not . isAvvmAddr) addrs))

-- | Returns true if the 'addrActorIx' is the 'IxAvvm' constructor.
isAvvmAddr :: Addr -> Bool
isAvvmAddr addr =
    case addrActorIx addr of
        IxAvvm _ -> True
        _ -> False

-- | Select a random address from the current set of addresses, and
-- a random value between 0 and that address's total value minus the
-- possible fee amount. The 'Value' that is returned is not deducted from
-- the 'Addr's balance.
selectFromAddrAndAmount :: BlockGen h (Addr, Value)
selectFromAddrAndAmount = do
    addrs <- uses bgcAddressesToBalances Map.toList
    (addr, amount) <- liftGen $ elements addrs
    value <- liftGen $ choose (1, amount `safeSubtract` maxFee)
    if value < 1
        then localBlockGen
            (bgcAddressesToBalances . at addr .~ Nothing)
            selectFromAddrAndAmount
        else pure (addr, value)

maxFee :: Num a => a
maxFee = 180000

-- | Run a 'BlockGen' action with a modified context. Changes to the state
-- from the inner action are discarded.
localBlockGen
    :: (BlockGenCtx h -> BlockGenCtx h)
    -> BlockGen h a
    -> BlockGen h a
localBlockGen f action = do
    st <- get
    modify f
    r <- action
    put st
    pure r

-- | 'Value' is an alias for 'Word64', which underflows. This detects
-- underflow and returns @0@ for underflowing values.
safeSubtract :: Value -> Value -> Value
safeSubtract x y
    | z > x     = 0
    | otherwise = z
  where
    z = x - y

-- | Create a fresh transaction that depends on the fee provided to it.
newTransaction :: Hash h Addr => BlockGen h (Value -> Transaction h Addr)
newTransaction = do
    (fromAddr, amount) <- selectFromAddrAndAmount
    toAddr <- selectToAddress fromAddr
    hash' <- freshHash
    txns <- uses bgcCurrentBlockchain ledgerToNewestFirst
    bgcAddressesToBalances . ix fromAddr -= amount
    (inputs, change) <-
        case createInputsAmountingTo fromAddr txns amount of
            Nothing ->
                error $ unlines
                    [ "Somehow doesn't have enough balance? "
                    , show amount
                    , show fromAddr
                    , show toAddr
                    ]
            Just (inputs, change) ->
                pure (inputs, change)
    let txn fee = Transaction
            { trFresh = 0
            , trFee = fee
            , trHash = hash'
            , trIns = Set.fromList inputs
            , trOuts = maybeToList change ++ [Output toAddr amount]
            }

    -- we assume that the fee is 0 for initializing these transactions
    bgcCurrentBlockchain %= ledgerAdd (txn 0)
    pure txn

-- | Create a list of inputs (and values) as well as a "change" 'Output'.
-- If this function returns @Just (inputs, output)@, then the @inputs@ will
-- contain enough value to cover the requested @desiredAmount@ along with
-- an 'Output' that covers the difference between the requested amount and
-- the total that the inputs are good for.
createInputsAmountingTo
    :: Hash h Addr
    => Addr -- ^ The address that receives transactions
    -> [Transaction h Addr] -- ^ The ledger
    -> Value -- ^ The amount to acquire inputs for
    -> Maybe ([Input h Addr], Maybe (Output Addr))
createInputsAmountingTo addr txns desiredAmount =
    case foldr k (0, []) txns of
        (total, inputs)
            | total < desiredAmount ->
                Nothing
            | otherwise ->
                Just
                    ( inputs
                    , if isAvvmAddr addr
                        then Nothing
                        else Just 
                            $ Output addr 
                            ( total 
                            `safeSubtract` desiredAmount 
                            `safeSubtract` maxFee
                            )
                    )
  where
    k txn (amt, acc)
        | amt >= desiredAmount =
            (amt, acc)
        | otherwise =
            let outs  = trOuts txn
                ours  = filter ((addr ==) . outAddr . snd)
                      $ zip [0..] outs
                inpts = map (Input (hash txn) . fst) ours
                val   = sum (map (outVal . snd) ours)
             in (amt + val, inpts ++ acc)

newBlock :: Hash h Addr => BlockGen h [Value -> Transaction h Addr]
newBlock = do
    txnCount <- liftGen $ choose (1, 10)
    replicateM txnCount newTransaction

newChain :: Hash h Addr => BlockGen h [[Value -> Transaction h Addr]]
newChain = do
    blockCount <- liftGen $ choose (1, 10)
    replicateM blockCount newBlock

zipFees
    :: [[Value -> Transaction h Addr]]
    -> ([[Value]] -> [[Transaction h Addr]])
zipFees = zipWith (zipWith ($))
