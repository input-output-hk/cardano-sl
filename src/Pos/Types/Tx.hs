{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

-- | Transaction related functions.

module Pos.Types.Tx
       ( verifyTxAlone
       , verifyTx
       , verifyTxPure
       , topsortTxs
       ) where

import           Control.Lens        (makeLenses, use, uses, (%=), (.=), (^.))
import           Data.Bifunctor      (first)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.List           (tail, zipWith3)
import           Formatting          (build, int, sformat, (%))
import           Serokell.Util       (VerificationRes, verifyGeneric)
import           Universum

import           Pos.Binary.Types    ()
import           Pos.Crypto          (Hash, WithHash (..), checkSig, hash)
import           Pos.Script          (txScriptCheck)
import           Pos.Types.Address   (addressDetailedF)
import           Pos.Types.Types     (Tx (..), TxIn (..), TxInWitness (..), TxOut (..),
                                      TxWitness, checkPubKeyAddress, checkScriptAddress,
                                      coinF)

-- | Verify that Tx itself is correct. Most likely you will also want
-- to verify that inputs are legal, signed properly and have enough coins;
-- 'verifyTxAlone' doesn't do that.
verifyTxAlone :: Tx -> VerificationRes
verifyTxAlone Tx {..} =
    mconcat
        [ verifyGeneric
              [ (not (null txInputs), "transaction doesn't have inputs")
              , (not (null txOutputs), "transaction doesn't have outputs")
              ]
        , verifyOutputs
        ]
  where
    verifyOutputs = verifyGeneric $ zipWith outputPredicate [0..] txOutputs
    outputPredicate (i :: Word) TxOut{..} =
        ( txOutValue > 0
        , sformat
              ("output #"%int%" has non-positive value: "%coinF) i txOutValue)

-- | CHECK: Verify Tx correctness using magic function which resolves input
-- into Address and Coin. It does checks from 'verifyTxAlone' and the
-- following:
--
-- * sum of inputs >= sum of outputs;
-- * every input is signed properly;
-- * every input is a known unspent output.
verifyTx
    :: (Monad m)
    => (TxIn -> m (Maybe TxOut))
    -> (Tx, TxWitness)
    -> m VerificationRes
verifyTx inputResolver txs@(Tx {..}, _) =
    flip verifyTxDo txs <$> mapM extendInput txInputs
  where
    extendInput txIn = fmap (txIn, ) <$> inputResolver txIn

verifyTxDo :: [Maybe (TxIn, TxOut)] -> (Tx, TxWitness) -> VerificationRes
verifyTxDo extendedInputs (tx@Tx{..}, witnesses) =
    mconcat [verifyTxAlone tx, verifyCounts, verifySum, verifyInputs]
  where
    outSum :: Integer
    outSum = sum $ fmap (toInteger . txOutValue) txOutputs
    resolvedInputs = catMaybes extendedInputs
    inpSum :: Integer
    inpSum = sum $ fmap (toInteger . txOutValue . snd) resolvedInputs
    txOutHash = hash txOutputs
    verifyCounts =
        verifyGeneric
            [ ( length txInputs == length witnesses
              , sformat ("length of inputs != length of witnesses "%
                         "("%int%" != "%int%")")
                  (length txInputs) (length witnesses) )
            ]
    verifySum =
        let resInps = length resolvedInputs
            extInps = length extendedInputs
            allInputsExist = resInps == extInps
            verifier =
                if allInputsExist
                    then ( inpSum >= outSum
                         , sformat
                               ("sum of outputs is more than sum of inputs ("
                                %int%" > "%int)
                                outSum inpSum)
                    else ( False
                         , sformat
                               (int%" inputs could not be resolved")
                               (abs $ resInps - extInps))
        in verifyGeneric [verifier]
    verifyInputs =
        verifyGeneric $ concat $
            zipWith3 inputPredicates [0..] extendedInputs (toList witnesses)

    inputPredicates
        :: Word                     -- ^ Input index
        -> Maybe (TxIn, TxOut)      -- ^ Input and corresponding output
        -> TxInWitness
        -> [(Bool, Text)]
    inputPredicates i Nothing _ =
        [(False, sformat ("input #"%int%" is not an unspent output") i)]
    inputPredicates i (Just (txIn@TxIn{..}, txOut@TxOut{..})) witness =
        [ ( checkAddrHash txOutAddress witness
          , sformat ("input #"%int%"'s witness doesn't match address "%
                     "of corresponding output:\n"%
                     "  input: "%build%"\n"%
                     "  output spent by this input: "%build%"\n"%
                     "  address details: "%addressDetailedF%"\n"%
                     "  witness: "%build)
                i txIn txOut txOutAddress witness
          )
        , case validateTxIn txIn witness of
              Right _ -> (True, panic "can't happen")
              Left err -> (False, sformat
                  ("input #"%int%" isn't validated by its witness:\n"%
                   "  reason: "%build%"\n"%
                   "  input: "%build%"\n"%
                   "  output spent by this input: "%build%"\n"%
                   "  witness: "%build)
                  i err txIn txOut witness)
        ]

    checkAddrHash addr PkWitness{..}     = checkPubKeyAddress twKey addr
    checkAddrHash addr ScriptWitness{..} = checkScriptAddress twValidator addr

    validateTxIn TxIn{..} PkWitness{..} =
        if checkSig twKey (txInHash, txInIndex, txOutHash) twSig
            then Right ()
            else Left "signature check failed"
    validateTxIn TxIn{..} ScriptWitness{..} =
        txScriptCheck twValidator twRedeemer

verifyTxPure :: (TxIn -> Maybe TxOut) -> (Tx, TxWitness) -> VerificationRes
verifyTxPure resolver = runIdentity . verifyTx (Identity . resolver)

data TopsortState a = TopsortState
    { _tsVisited     :: HS.HashSet (Hash Tx)
    , _tsUnprocessed :: [a]
    , _tsResult      :: [a]
    , _tsLoop        :: Bool
    }

$(makeLenses ''TopsortState)

-- | Does topological sort on things that contain transactions – e.g. can be
-- used both for sorting @[Tx]@ and @[(Tx, TxWitness)]@.
--
-- (Backwards dfs from every node with reverse visiting order
-- recording. Returns nothing on loop encountered. Return order is
-- head-first.)
topsortTxs :: forall a. (a -> WithHash Tx) -> [a] -> Maybe [a]
topsortTxs toTx input =
    let res = execState dfs1 initState
    in guard (not $ res ^. tsLoop) >> pure (reverse $ res ^. tsResult)
  where
    dup a = (a,a)
    txHashes :: HashMap (Hash Tx) a
    txHashes = HM.fromList $ map (first (whHash . toTx) . dup) input
    initState = TopsortState HS.empty input [] False
    -- Searches next unprocessed vertix and calls dfs2 for it. Wipes
    -- visited vertices.
    dfs1 :: State (TopsortState a) ()
    dfs1 = unlessM (use tsLoop) $ do
        t <- uses tsUnprocessed head
        whenJust t $ \a -> do
            let tx = toTx a
            ifM (uses tsVisited $ HS.member (whHash tx))
                (tsUnprocessed %= tail)
                (dfs2 HS.empty a tx)
            dfs1
    -- Does dfs putting vertices into tsResult in reversed order of
    -- visiting. visitedThis is map of visited vertices for _this_ dfs
    -- (cycle detection).
    dfs2 :: HashSet (Hash Tx) -> a -> WithHash Tx -> State (TopsortState a) ()
    dfs2 visitedThis _ (WithHash _ txHash)
        | txHash `HS.member` visitedThis = tsLoop .= True
    dfs2 visitedThis a (WithHash tx txHash) = unlessM (use tsLoop) $ do
        tsVisited %= HS.insert txHash
        let visitedNew = HS.insert txHash visitedThis
            dependsUnfiltered =
                mapMaybe (\x -> HM.lookup (txInHash x) txHashes) (txInputs tx)
        depends <- filterM
            (fmap not . uses tsVisited . HS.member . whHash . toTx)
            dependsUnfiltered
        forM_ depends $ \a' -> dfs2 visitedNew a' (toTx a')
        tsResult %= (a:)
