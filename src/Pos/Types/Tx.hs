{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

-- | Transaction related functions.

module Pos.Types.Tx
       ( verifyTxAlone
       , verifyTx
       , topsortTxs
       ) where

import           Control.Lens        (makeLenses, use, uses, (%=), (.=), (^.))
import           Data.Bifunctor      (first)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.List           (tail)
import           Formatting          (build, int, sformat, (%))
import           Serokell.Util       (VerificationRes, verifyGeneric)
import           Universum

import           Pos.Crypto          (addressHash, hash, verify)
import           Pos.Types.Types     (Address (..), Redeemer (..), Tx (..), TxIn (..),
                                      TxOut (..), Validator (..), coinF)

-- | Verify that Tx itself is correct. Most likely you will also want
-- to verify that inputs are legal, signed properly and have enough coins.
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

-- | Verify Tx correctness using magic function which resolves input
-- into Address and Coin. It does checks from verifyTxAlone and the
-- following:
--
-- * sum of inputs ≥ sum of outputs;
-- * every input is signed properly;
-- * every input is a known unspent output.
verifyTx :: (TxIn -> Maybe TxOut) -> Tx -> VerificationRes
verifyTx inputResolver tx@Tx {..} =
    mconcat [verifyTxAlone tx, verifySum, verifyInputs]
  where
    outSum :: Integer
    outSum = sum $ fmap (toInteger . txOutValue) txOutputs
    extendedInputs :: [Maybe (TxIn, TxOut)]
    extendedInputs = fmap extendInput txInputs
    extendInput txIn = (txIn,) <$> inputResolver txIn
    inpSum :: Integer
    inpSum = sum $ fmap (toInteger . txOutValue . snd) $ catMaybes extendedInputs
    verifySum =
        verifyGeneric
            [ ( inpSum >= outSum
              , sformat
                    ("sum of outputs is more than sum of inputs ("
                     %int%" > "%int%"), maybe some inputs are invalid")
                    outSum inpSum)
            ]
    verifyInputs =
        verifyGeneric $ concat $ zipWith inputPredicates [0..] extendedInputs

    inputPredicates :: Word -> Maybe (TxIn, TxOut) -> [(Bool, Text)]
    inputPredicates i Nothing =
        [(False, sformat ("input #" %int% " is not an unspent output: ") i)]
    inputPredicates i (Just (txIn@TxIn{..}, TxOut{..})) =
        [ ( checkAddrHash txOutAddress txInValidator
          , sformat ("input #"%int%" doesn't match address "
                     %build%" of corresponding output: ("%build%")") i txOutAddress txIn
          )
        , ( validateTxIn txIn
          , sformat ("input #"%int%" is not signed properly: ("%build%")") i txIn
          )
        ]

    checkAddrHash addr (PubKeyValidator pk) = addrHash addr == addressHash pk
    validateTxIn TxIn{..} =
        let pk = getValidator txInValidator
            sig = getRedeemer txInRedeemer
        in verify pk (txInHash, txInIndex, txOutputs) sig

data TopsortState = TopsortState
    { _tsVisited     :: HS.HashSet Tx
    , _tsUnprocessed :: [Tx]
    , _tsResult      :: [Tx]
    , _tsLoop        :: Bool
    }

$(makeLenses ''TopsortState)

-- | Does topological sort on transactions -- backwards dfs from every
-- node with reverse visiting order recording. Returns nothing on loop
-- encountered. Return order is head-first.
topsortTxs :: [Tx] -> Maybe [Tx]
topsortTxs input =
    let res = execState dfs1 initState
    in guard (not $ res ^. tsLoop) >> pure (reverse $ res ^. tsResult)
  where
    dup a = (a,a)
    txHashes = HM.fromList $ map (first hash . dup) input
    initState = TopsortState HS.empty input [] False
    -- Searches next unprocessed vertix and calls dfs2 for it. Wipes
    -- visited vertices.
    dfs1 :: State TopsortState ()
    dfs1 = unlessM (use tsLoop) $ do
        t <- uses tsUnprocessed head
        whenJust t $ \k -> do
            ifM (uses tsVisited $ HS.member k)
                (tsUnprocessed %= tail)
                (dfs2 HS.empty k)
            dfs1
    -- Does dfs putting vertices into tsResult in reversed order of
    -- visiting. visitedThis is map of visited vertices for _this_ dfs
    -- (cycle detection).
    dfs2 visitedThis tx@Tx{..} | tx `HS.member` visitedThis = tsLoop .= True
    dfs2 visitedThis tx@Tx{..} = unlessM (use tsLoop) $ do
        tsVisited %= HS.insert tx
        let visitedNew = HS.insert tx visitedThis
            dependsUnfiltered =
                mapMaybe (\x -> HM.lookup (txInHash x) txHashes) txInputs
        depends <-
            filterM (fmap not . uses tsVisited . HS.member) dependsUnfiltered
        forM_ depends $ dfs2 visitedNew
        tsResult %= (tx:)
