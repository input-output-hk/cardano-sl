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
import           Formatting                 (build, sformat, (%))
import           System.Random              (RandomGen (..))

import           Pos.AllSecrets             (asSecretKeys, asSpendingData,
                                             unInvAddrSpendingData, unInvSecretsMap)
import           Pos.Client.Txp.Util        (createGenericTx, makeMPubKeyTxAddrs,
                                             unTxError)
import           Pos.Core                   (AddrSpendingData (..), Address (..), Coin,
                                             SlotId (..), StakeholderId, addressHash,
                                             coinToInteger, makePubKeyAddress,
                                             unsafeIntegerToCoin)
import           Pos.Crypto                 (SecretKey, WithHash (..), fakeSigner, hash,
                                             toPublic)
import           Pos.Generator.Block.Error  (BlockGenError (..))
import           Pos.Generator.Block.Mode   (BlockGenRandMode, MonadBlockGenBase)
import           Pos.Generator.Block.Param  (HasBlockGenParams (..), HasTxGenParams (..))
import qualified Pos.GState                 as DB
import           Pos.Txp.Core               (Tx (..), TxAux (..), TxIn (..), TxOut (..),
                                             TxOutAux (..))
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Local     (eTxProcessTransaction)
#else
import           Pos.Txp.Logic              (txProcessTransaction)
#endif
import           Pos.Txp.Toil.Class         (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Types         (Utxo)
import qualified Pos.Txp.Toil.Utxo          as Utxo
import           Pos.Util.Util              (maybeThrow)

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

-- | Selects some number of distinct elements from the list; the number is
-- taken from given range [a, b]
selectSomeFromList :: MonadRandom m => (Int, Int) -> [a] -> m [a]
selectSomeFromList p@(a, b0) ls
    | b - a < 0 = error $ "selectSomeFromList: b < a " <> show p
    | otherwise = do
          n <- getRandomR (a, b)
          idxs <- selectDistinct n (0, l - 1)
          pure $ map (ls !!) idxs
  where
    l = length ls
    b = min l b0

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
        -- Just an arbitrary not-so-big number of attempts to fit predicates
        -- to avoid infinite loops
        -- let randomAttempts :: Int
            -- randomAttempts = 20
        utxo <- use gtdUtxo
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
            utxoAddrsN = HM.size secrets
        let adder hm TxOutAux { toaOut = TxOut {..} } =
                HM.insertWith (+) txOutAddress (coinToInteger txOutValue) hm
            utxoBalances = foldl' adder mempty utxo
            hasMoney addr = fromMaybe 0 (HM.lookup addr utxoBalances) > 0
            addrsWithMoney = filter hasMoney utxoAddresses

        -- Select input and output addresses

        inputAddrs <- selectSomeFromList (1, 3) addrsWithMoney
        outputAddrs <- selectSomeFromList (1, 3) utxoAddresses
        let outputsN = length outputAddrs

        -- Select UTXOs belonging to one of input addresses and determine
        -- total amount of money available
        let ownUtxo = Utxo.filterUtxoByAddrs inputAddrs utxo
            totalOwnMoney = coinToInteger $ Utxo.getTotalCoinsInUtxo ownUtxo

        -- We divide total own money by 2 to have space for tx fee
        totalTxAmount <- getRandomR (fromIntegral outputsN, totalOwnMoney `div` 2)

        changeAddrIdx <- getRandomR (0, utxoAddrsN - 1)
        let changeAddrData = makePubKeyAddress &&& (Just . addressHash) $
                toPublic $ HM.elems secrets !! changeAddrIdx

        -- Prepare tx outputs
        coins <- splitCoins outputsN (unsafeIntegerToCoin totalTxAmount)
        let txOuts :: NonEmpty TxOut
            txOuts = NE.fromList $ zipWith TxOut outputAddrs coins
        let txOutToOutAux txOut@(TxOut addr coin) = do
                sk <- addrToSk addr
                let sId :: StakeholderId
                    sId = addressHash (toPublic sk)
                let distr = one (sId, coin)
                return TxOutAux { toaOut = txOut, toaDistr = distr }
        txOutAuxs <- mapM txOutToOutAux txOuts

        -- Form a transaction
        inputSKs <- mapM addrToSk inputAddrs
        let hdwSigners = NE.fromList $ zip (map fakeSigner inputSKs) inputAddrs
            makeTestTx = makeMPubKeyTxAddrs hdwSigners

        eTx <- lift . lift $
            createGenericTx makeTestTx ownUtxo txOutAuxs changeAddrData
        (txAux, _) <- either (throwM . BGFailedToCreate . unTxError) pure eTx

        let tx = taTx txAux
        let txId = hash tx
        let txIns = _txInputs tx
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
                        map (TxIn txId) [0..(fromIntegral $ length txOuts)-1]
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
