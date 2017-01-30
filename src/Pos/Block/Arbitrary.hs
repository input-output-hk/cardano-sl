{-# LANGUAGE UndecidableInstances #-}

module Pos.Block.Arbitrary
       ( BlockHeaderList (..)
       ) where

import           Data.Ix              (range)
import           Data.Text.Buildable  (Buildable)
import qualified Data.Text.Buildable  as Buildable
import           Formatting           (bprint, build, formatToString, (%))
import           Test.QuickCheck      (Arbitrary (..), Gen, choose, listOf, oneof,
                                       vectorOf)
import           Universum

import           Pos.Binary           (Bi)
import           Pos.Block.Network    as T
import           Pos.Constants        (epochSlots)
import           Pos.Crypto           (Hash, ProxySecretKey, PublicKey, SecretKey,
                                       createProxySecretKey, toPublic)
import           Pos.Data.Attributes  (Attributes (..), mkAttributes)
import           Pos.Merkle           (MerkleRoot (..), MerkleTree, mkMerkleTree)
import           Pos.Ssc.Class        (Ssc (..), SscHelpersClass)
import qualified Pos.Types            as T
import           Pos.Update.Arbitrary ()
import           Pos.Util             (Raw, makeSmall)
import qualified Prelude

------------------------------------------------------------------------------------------
-- Arbitrary instances for Blockchain related types
------------------------------------------------------------------------------------------

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.BlockSignature ssc) where
    arbitrary = oneof [ T.BlockSignature <$> arbitrary
                      , T.BlockPSignatureEpoch <$> arbitrary
                      , T.BlockPSignatureSimple <$> arbitrary
                      ]

properBlock
    :: ( Arbitrary (T.BHeaderHash b)
       , Arbitrary (T.Body b)
       , Arbitrary (T.ConsensusData b)
       , Arbitrary (T.ExtraBodyData b)
       , Arbitrary (T.ExtraHeaderData b)
       , T.Blockchain b)
    => Gen (T.GenericBlock b)
properBlock = do
    body <- arbitrary
    (prevBlock, consensus, extra) <- arbitrary
    let proof = T.mkBodyProof body
        header = T.GenericBlockHeader prevBlock proof consensus extra
    T.GenericBlock <$> pure header <*> pure body <*> arbitrary

------------------------------------------------------------------------------------------
-- GenesisBlockchain
------------------------------------------------------------------------------------------

instance Arbitrary (T.GenesisBlockHeader ssc) where
    arbitrary = T.GenericBlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary (T.BodyProof (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisProof <$> arbitrary

instance Arbitrary (T.ConsensusData (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisConsensusData
        <$> arbitrary
        <*> arbitrary

instance Arbitrary (T.Body (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisBody <$> arbitrary

instance Arbitrary (T.GenericBlock (T.GenesisBlockchain ssc)) where
    arbitrary = properBlock

------------------------------------------------------------------------------------------
-- MainBlockchain
------------------------------------------------------------------------------------------

instance Bi Raw => Arbitrary (MerkleRoot T.Tx) where
    arbitrary = MerkleRoot <$> (arbitrary :: Gen (Hash Raw))

instance Arbitrary (MerkleTree T.Tx) where
    arbitrary = mkMerkleTree <$> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.MainBlockHeader ssc) where
    arbitrary = T.GenericBlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary h => Arbitrary (Attributes h) where
    arbitrary = Attributes
        <$> arbitrary
        <*> arbitrary

instance Arbitrary T.MainExtraHeaderData where
    arbitrary = T.MainExtraHeaderData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary T.MainExtraBodyData where
    arbitrary = T.MainExtraBodyData <$> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw) =>
    Arbitrary (T.BodyProof (T.MainBlockchain ssc)) where
    arbitrary = T.MainProof
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.ConsensusData (T.MainBlockchain ssc)) where
    arbitrary = T.MainConsensusData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

-- | In the main blockchain's body, the number of transactions must be the same as the
-- number of transaction witnesses.
--
-- Furthermore, for every transaction in index i of the list, the length of its output
-- list must be the same as the length of the i-th item in the TxDistribution list.
--
-- Because of this, the Arbitrary instance for Ssc ssc => Body (MainBlockchain ssc)
-- ensures that for every transaction generated, a transaction witness is generated as
-- well, and the lengths of its list of outputs must also be the same as the length of its
-- corresponding TxDistribution item.

txOutDistGen :: Gen [(T.Tx, T.TxDistribution, T.TxWitness)]
txOutDistGen = listOf $ do
    txInW <- arbitrary
    txIns <- arbitrary
    (txOuts, txDist) <- second T.TxDistribution . unzip <$> arbitrary
    return $ (T.Tx txIns txOuts $ mkAttributes (), txDist, txInW)

instance Arbitrary (SscPayload ssc) => Arbitrary (T.Body (T.MainBlockchain ssc)) where
    arbitrary = makeSmall $ do
        (txList, txDists, txInW) <- unzip3 <$> txOutDistGen
        mpcData     <- arbitrary
        mpcProxySKs <- arbitrary
        mpcUpload   <- arbitrary
        return $ T.MainBody (mkMerkleTree txList) txDists txInW mpcData mpcProxySKs mpcUpload

instance (Arbitrary (SscProof ssc), Arbitrary (SscPayload ssc), SscHelpersClass ssc) =>
    Arbitrary (T.GenericBlock (T.MainBlockchain ssc)) where
    arbitrary = properBlock

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance Arbitrary T.MsgGetHeaders where
    arbitrary = T.MsgGetHeaders
        <$> arbitrary
        <*> arbitrary

instance Arbitrary T.MsgGetBlocks where
    arbitrary = T.MsgGetBlocks
        <$> arbitrary
        <*> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) => Arbitrary (T.MsgHeaders ssc) where
    arbitrary = T.MsgHeaders <$> arbitrary

instance (Arbitrary (SscProof ssc), Arbitrary (SscPayload ssc), SscHelpersClass ssc) =>
    Arbitrary (T.MsgBlock s ssc) where
    arbitrary = T.MsgBlock <$> arbitrary

instance T.BiSsc ssc => Buildable (T.BlockHeader ssc, PublicKey) where
    build (block, key) =
        bprint
            ( build%"\n"%
              build%"\n"
            ) block key

newtype BlockHeaderList ssc = BHL
    { getHeaderList :: ([T.BlockHeader ssc], [PublicKey])
    } deriving (Eq)

instance T.BiSsc ssc => Show (BlockHeaderList ssc) where
    show =
        concat . intersperse "\n" . map (formatToString build) . uncurry zip . getHeaderList

-- | Starting Epoch in block header verification tests
startingEpoch :: Integral a => a
startingEpoch = 0

-- | Maximum permitted epoch in block header verification tests
maxEpochs :: Integral a => a
maxEpochs = 0

-- | Generation of arbitrary, valid headerchain along with a list of leaders for
-- each epoch.
--
-- Because `verifyHeaders` assumes the head of the list is the most recent
-- block, this function is tail-recursive: while keeping track of the current
-- block and epoch/slot, it adds the most recent one to the head of the header list
-- it'll return when done.
--
-- The `[Either SecretKey (SecretKey, SecretKey, Bool)]` type is for determining what
-- kind of signature the slot's block will have. If it's `Left sk`, it'll be a simple
-- `BlockSignature`; if it's `Right (issuerSK, delegateSK, b)`, it will be a proxy
-- signature, and if `b :: Bool` is false, it'll be a simple proxy secret key.
-- Otherwise, it'll be a proxy secret key with epochs, whose lower and upper epoch
-- bounds will be randomly generated.
--
-- Beware that
-- * genesis blocks have no leaders, and that
-- * if an epoch is `n` slots long, every `n+1`-th block will be of the genesis kind.


recursiveHeaderGen
    :: (Arbitrary (SscPayload ssc), SscHelpersClass ssc, Integral a)
    => [Either SecretKey (SecretKey, SecretKey, Bool)]
    -> [(a, a)]
    -> [T.BlockHeader ssc]
    -> Gen [T.BlockHeader ssc]
recursiveHeaderGen
    (eitherOfLeader : leaders)
    ((epoch, slot) : rest)
    blockchain@(prevHeader : _) = do
        let epochCounter = fromIntegral epoch
            slotCounter = fromIntegral slot
        curHeader <- do
            if slot == epochSlots
                then do
                    body <- arbitrary
                    return $ Left $ T.mkGenesisHeader (Just prevHeader) (epochCounter + 1) body
                else do
                    body <- arbitrary
                    extraHData <- arbitrary
                    lowEpoch <- choose (0, epochCounter)
                    highEpoch <- choose (epochCounter, maxEpochs + 1)
                    -- These two values may not be used at all. If the slot in question
                    -- will have a simple signature, laziness will prevent them from
                    -- being calculated. Otherwise, they'll be the proxy secret key's ω.
                    let slotId = T.SlotId epochCounter slotCounter
                        (leader, proxySK) =
                            case eitherOfLeader of
                                Left sk -> (sk, Nothing)
                                Right (issuerSK, delegateSK, isSigEpoch) ->
                                    let w = (lowEpoch, highEpoch)
                                        delegatePK = toPublic delegateSK
                                        curried :: Bi w => w -> ProxySecretKey w
                                        curried = createProxySecretKey issuerSK delegatePK
                                        proxy = if isSigEpoch
                                             then Right  $ curried ()
                                             else Left $ curried w
                                    in (delegateSK, Just $ proxy)
                    return $ Right $ T.mkMainHeader (Just prevHeader) slotId leader proxySK body extraHData
        recursiveHeaderGen leaders rest (curHeader : blockchain)
recursiveHeaderGen [] _ b = return b
recursiveHeaderGen _ [] b = return b
recursiveHeaderGen _ _ _  = return []


-- | This type is used to generate a blockchain, as well a list of leaders for every
-- slot with which the chain will be paired. The leaders are in reverse order to the
-- chain - the list goes from first to last leader. This is used in a `verifyHeader`
-- test.
--
-- Note that every non-empty blockchain has at least one epoch, which may be complete
-- or incomplete. To simulate this behavior, two random numbers are generated:
-- one that stands for the number of complete epochs we have, and the other for the
-- number of incomplete slots of the last epoch, which, in this instance, must exist.
--
-- A blockchain with only complete epochs is a subset of some blockchain with one
-- incomplete epoch, so if the former is desired, a simple list `takeWhile` of the list
-- this instance generates will be enough.
--
-- Note that a leader is generated for each slot.
-- (Not exactly a leader - see previous comment)

instance (Arbitrary (SscPayload ssc), SscHelpersClass ssc) => Arbitrary (BlockHeaderList ssc) where
    arbitrary = BHL <$> do
        fullEpochs <- choose (startingEpoch, maxEpochs)
        incompleteEpochSize <- T.LocalSlotIndex <$> choose (1, epochSlots - 1)
        leadersList <-
            vectorOf ((epochSlots * fullEpochs) + fromIntegral incompleteEpochSize)
                arbitrary
        firstGenesisBody <- arbitrary
        let firstHeader = Left $ T.mkGenesisHeader Nothing startingEpoch firstGenesisBody
            actualLeaders = map (toPublic . either identity (view _1)) leadersList
        (, actualLeaders) <$>
            recursiveHeaderGen
                leadersList
                (range ((startingEpoch, 0), (fromIntegral fullEpochs, epochSlots)) ++
                zip (repeat $ fromIntegral (fullEpochs + 1)) [0 .. incompleteEpochSize - 1])
                -- This `range` will give us pairs with all complete epochs and for each,
                -- every slot therein.
                [firstHeader]
