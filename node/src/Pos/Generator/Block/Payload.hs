{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- TODO Maybe move it somewhere else.
-- | Block payload generation.

module Pos.Generator.Block.Payload
       ( genPayload
       ) where

import           Universum

import           Control.Lens               (at, uses, (%=), (.=), (?=))
import           Control.Lens.TH            (makeLenses)
import           Control.Monad.Random.Class (MonadRandom (..))
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (notElem, (!!))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Vector                as V
import           Ether.Internal             (HasLens (..))
import           Formatting                 (build, sformat, (%))
import           System.Random              (RandomGen (..))

import           Pos.AllSecrets             (asSecretKeys, asSpendingData,
                                             unInvAddrSpendingData, unInvSecretsMap)
import           Pos.Client.Txp.Util        (makeAbstractTx, overrideTxDistrBoot,
                                             runTxCreator, txToLinearFee, unTxError)
import           Pos.Core                   (AddrSpendingData (..), Address (..), Coin,
                                             SlotId (..), StakeholderId, TxFeePolicy (..),
                                             addressHash, bvdTxFeePolicy, coinToInteger,
                                             makePubKeyAddress, mkCoin, sumCoins,
                                             unsafeGetCoin, unsafeIntegerToCoin)
import           Pos.Crypto                 (SecretKey, SignTag (SignTx), WithHash (..),
                                             hash, sign, toPublic)
import           Pos.DB                     (gsIsBootstrapEra)
import           Pos.Generator.Block.Error  (BlockGenError (..))
import           Pos.Generator.Block.Mode   (BlockGenRandMode, MonadBlockGenBase)
import           Pos.Generator.Block.Param  (HasBlockGenParams (..), HasTxGenParams (..))
import           Pos.Genesis                (GenesisWStakeholders (..), bootDustThreshold)
import qualified Pos.GState                 as DB
import           Pos.Slotting.Class         (MonadSlots (getCurrentSlotBlocking))
import           Pos.Txp.Core               (TxAux (..), TxIn (..), TxInWitness (..),
                                             TxOut (..), TxOutAux (..), TxSigData (..))
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Local     (eTxProcessTransaction)
#else
import           Pos.Txp.Logic              (txProcessTransaction)
#endif
import           Pos.Txp.Toil.Class         (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Types         (TxFee (..), Utxo)
import qualified Pos.Txp.Toil.Utxo          as Utxo
import           Pos.Util.Util              (eitherToThrow, maybeThrow)

----------------------------------------------------------------------------
-- Tx payload generation
----------------------------------------------------------------------------

-- | Generates list of distinct ints in the given range [a,b].
selectDistinct :: forall m. (MonadRandom m) => Int -> (Int,Int) -> m [Int]
selectDistinct n0 p@(a, b)
    | b - a < 0 = error $ "selectDistinct: b < a " <> show p
    | otherwise = do
          res <- reverse <$> selectDistinct' [] n (a, b)
          if fromIntegral (length res) /= n
              then error $ "selectDistinct is broken: " <> show res <> " " <> show n
              else pure res
  where
    n :: Int
    n = min (b + 1 - a) n0
    selectDistinct' :: [Int] -> Int -> (Int, Int) -> m [Int]
    selectDistinct' cur 0 _ = pure cur
    selectDistinct' cur leftN (a', b') = do
        let upEdge = b' - leftN + 1
        nextInt <- getRandomR (a', upEdge)
        selectDistinct' (nextInt : cur) (leftN - 1) (nextInt + 1, b')

-- | Separates coin into provided number of coins. All resulting coins
-- are nonzero.
splitCoins :: (MonadRandom m) => Int -> Coin -> m [Coin]
splitCoins n c0
    | c == (0::Int) = error "splitCoins, c = 0"
    | c < n = error $ "splitCoins: can't split " <> pretty c0 <>
                      " on " <> show n <> " parts"
    | c == (1::Int) = pure [c0]
    | otherwise = do
          splitPoints <- sort <$> selectDistinct (n-1) (1, c - 1)
          -- calculate length of intervals
          let amounts = map (\(a,b) -> b - a) $
                            (0 : splitPoints) `zip` (splitPoints ++ [c])
          -- here we can use unsafeIntegerToCoin, because amount of
          -- subcoin is less than 'c' by design.
          pure $ map (unsafeIntegerToCoin . fromIntegral) amounts
  where
    c :: Integral a => a
    c = fromIntegral $ coinToInteger c0

-- | State datatype for transaction payload generation
data GenTxData = GenTxData
    { _gtdUtxo     :: Utxo
      -- ^ Utxo as it is.
    , _gtdUtxoKeys :: V.Vector TxIn
      -- ^ Keys of 'gtdUtxo' to support random selection by index.
    }

makeLenses ''GenTxData

instance (Monad m) => MonadUtxoRead (StateT GenTxData m) where
    utxoGet txIn = uses gtdUtxo $ M.lookup txIn

instance (Monad m) => MonadUtxo (StateT GenTxData m) where
    utxoPut txIn txOutAux = gtdUtxo . at txIn ?= txOutAux
    utxoDel txIn = gtdUtxo . at txIn .= Nothing

-- TODO: move to txp, think how to unite it with 'Pos.Arbitrary.Txp'.
-- | Generate valid 'TxPayload' using current global state.
genTxPayload ::
       forall g m. (RandomGen g, MonadBlockGenBase m)
    => BlockGenRandMode g m ()
genTxPayload = do
    utxo <- lift DB.getAllPotentiallyHugeUtxo
    let gtd = GenTxData utxo (V.fromList $ M.keys utxo)
    flip evalStateT gtd $ do
        (a,d) <- lift $ view tgpTxCountRange
        txsN <- fromIntegral <$> getRandomR (a, a + d)
        void $ replicateM txsN genTransaction
  where
    genTransaction :: StateT GenTxData (BlockGenRandMode g m) ()
    genTransaction = do
        epoch <- siEpoch <$> lift (lift getCurrentSlotBlocking)
        bootEra <- lift . lift $ gsIsBootstrapEra epoch
        genWStakeholders <- view (lensOf @GenesisWStakeholders)
        let dustThd :: Integral a => a
            dustThd = fromIntegral $ unsafeGetCoin $ bootDustThreshold genWStakeholders
        -- Just an arbitrary not-so-big number of attempts to fit predicates
        -- to avoid infinite loops
        let randomAttempts :: Int
            randomAttempts = 20
        -- Number of attempts to set a stable fee for transaction
        -- (the same as in `Pos.Client.Txp.Util.stabilizeTxFee`)
        let feeAttempts :: Int
            feeAttempts = 5
        utxoSize <- uses gtdUtxoKeys V.length
        when (utxoSize == 0) $
            lift $ throwM $ BGInternal "Utxo is empty when trying to create tx payload"

        secrets <- unInvSecretsMap . view asSecretKeys <$> view blockGenParams
        let resolveSecret :: MonadThrow n => StakeholderId -> n SecretKey
            resolveSecret stId =
                maybeThrow (BGUnknownSecret stId) (HM.lookup stId secrets)

        invAddrSpendingData <- unInvAddrSpendingData <$>
            view (blockGenParams . asSpendingData)
        let addrToSk :: MonadThrow n => Address -> n SecretKey
            addrToSk addr = do
                spendingData <- maybeThrow (BGUnknownAddress addr)
                    (invAddrSpendingData ^. at addr)
                case spendingData of
                    PubKeyASD pk -> resolveSecret (addressHash pk)
                    another -> error $
                        sformat ("Found an address with non-pubkey spending data: "
                                    %build) another

        let utxoAddresses = map (makePubKeyAddress . toPublic) $ HM.elems secrets

        ----- INPUTS

        let generateInputs attempts expectedFee@(TxFee fee) = do
                when (attempts <= 0) $
                    throwM . BGFailedToCreate $ "Too many attempts to choose tx inputs!"
                inputsN <- getRandomR (1, min 5 utxoSize)
                inputsIxs <- selectDistinct inputsN (0, utxoSize - 1)
                -- It's alright to use unsafeIndex because length of
                -- gtdUtxoKeys must match utxo size and inputsIxs is selected
                -- prior to length limitation.
                txIns <- forM inputsIxs $ \i -> uses gtdUtxoKeys (`V.unsafeIndex` i)
                inputsResolved <- forM txIns $ \txIn ->
                    -- we're selecting from utxo by 'gtdUtxoKeys'. Inability to resolve
                    -- txin means that 'GenTxData' is malformed.
                    toaOut .
                    fromMaybe (error "genTxPayload: inputsSum can't happen") <$>
                    utxoGet txIn
                let (inputsSum :: Integer) = sumCoins $ map txOutValue inputsResolved
                    minInputsSum = coinToInteger fee + 1
                -- We need to ensure that sum of inputs is enough to
                -- pay expected fee and leave some money for outputs
                if inputsSum < minInputsSum
                    -- just retry
                    then generateInputs (attempts - 1) expectedFee
                    else pure (txIns, inputsResolved, inputsSum)

        ----- OUTPUTS

        let generateOutputs inputsSum (TxFee fee) = do
                let outputsSum = inputsSum - coinToInteger fee
                -- this is max number of outputs such that none of
                -- them is less than dust treshold
                let ceilBoot = outputsSum `div` dustThd
                outputsMaxN <-
                    bool identity (min ceilBoot) bootEra .
                    fromIntegral .
                    max 1 <$>
                    lift (view tgpMaxOutputs)
                (outputsN :: Int) <-
                    fromIntegral <$> getRandomR (1, min outputsMaxN outputsSum)
                outputsIxs <-
                    selectDistinct
                        outputsN
                        (0, max outputsN (length utxoAddresses - 1))
                let outputAddrs = map ((cycle utxoAddresses) !!) outputsIxs
                -- We operate small coins values so any input sum mush be less
                -- than coin maxbound.
                coins <- splitCoins outputsN (unsafeIntegerToCoin outputsSum)
                let txOuts :: NonEmpty TxOut
                    txOuts = NE.fromList $ zipWith TxOut outputAddrs coins
                let txOutToOutAux txOut@(TxOut addr coin) = do
                        sk <- addrToSk addr
                        let sId :: StakeholderId
                            sId = addressHash (toPublic sk)
                        let distr = one (sId, coin)
                        return TxOutAux { toaOut = txOut, toaDistr = distr }
                txOutAuxsPre <- mapM txOutToOutAux txOuts
                either (lift . throwM . BGFailedToCreate . unTxError) pure =<<
                    runTxCreator (overrideTxDistrBoot txOutAuxsPre)

        ----- TX

        feePolicy <- lift . lift $ bvdTxFeePolicy <$> DB.getAdoptedBVData
        linearPolicy <- case feePolicy of
            TxFeePolicyUnknown w _ -> throwM . BGFailedToCreate $
                sformat ("Unknown fee policy, tag: "%build) w
            TxFeePolicyTxSizeLinear linear -> pure linear

        let genTxWithFee attempt expectedFee = do
                when (attempt <= 0) $
                    lift . throwM $ BGFailedToCreate "Too many attempts to set a tx fee!"
                (txIns, inputsResolved, inputsSum) <- generateInputs randomAttempts expectedFee
                txOutAuxs <- generateOutputs inputsSum expectedFee

                resolvedSks <- mapM (addrToSk . txOutAddress) inputsResolved
                let txInsWithSks = NE.fromList $ resolvedSks `zip` txIns
                let mkWit :: SecretKey -> TxSigData -> TxInWitness
                    mkWit sk txSigData = PkWitness (toPublic sk) (sign SignTx sk txSigData)
                let txAux = makeAbstractTx mkWit txInsWithSks txOutAuxs
                txFee <- lift $ eitherToThrow . first (BGFailedToCreate . unTxError) $
                    txToLinearFee linearPolicy txAux
                if txFee == expectedFee
                    then pure (txAux, txIns, txOutAuxs)
                    else genTxWithFee (attempt - 1) txFee

        (txAux, txIns, txOutAuxs) <- genTxWithFee feeAttempts (TxFee $ mkCoin 0)
        let tx = taTx txAux
        let txId = hash tx
#ifdef WITH_EXPLORER
        res <- lift . lift $ runExceptT $ eTxProcessTransaction (txId, txAux)
#else
        res <- lift . lift $ runExceptT $ txProcessTransaction (txId, txAux)
#endif
        case res of
            Left e  -> error $ "genTransaction@txProcessTransaction: got left: " <> pretty e
            Right _ -> do
                Utxo.applyTxToUtxo (WithHash tx txId) (taDistribution txAux)
                gtdUtxoKeys %= V.filter (`notElem` txIns)
                let outsAsIns =
                        map (TxIn txId) [0..(fromIntegral $ length txOutAuxs)-1]
                gtdUtxoKeys %= (V.++) (V.fromList outsAsIns)


----------------------------------------------------------------------------
-- Payload generation
----------------------------------------------------------------------------

-- Generate random payload which is valid with respect to the current
-- global state and mempool and add it to mempool.  Currently we are
-- concerned only about tx payload, later we can add more stuff.
genPayload ::
       forall g m.
       (RandomGen g, MonadBlockGenBase m)
    => SlotId
    -> BlockGenRandMode g m ()
genPayload _ = genTxPayload
