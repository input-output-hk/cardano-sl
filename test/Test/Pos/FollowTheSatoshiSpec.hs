{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specification of Pos.FollowTheSatoshi

module Test.Pos.FollowTheSatoshiSpec
       ( spec
       ) where

import           Crypto.Random            (MonadRandom)
import qualified Data.HashMap.Strict      as HM
import           Data.List                (foldl1', unzip, (\\))
import           Formatting               (build, int, sformat, (%))
import           Serokell.Util            (listJson)
import           Test.Hspec               (Spec, describe, pending)
import           Test.Hspec.QuickCheck    (modifyMaxSize, modifyMaxSuccess, prop)
import           Test.QuickCheck          (Property, choose, counterexample, generate,
                                           ioProperty, property, sized, (===))
import           Test.QuickCheck.Property (failed, succeeded)
import           Universum

import           Pos.Crypto               (KeyPair (..), Share, Threshold, VssKeyPair,
                                           decryptShare, sign, toVssPublicKey)
import           Pos.FollowTheSatoshi     (FtsError (..), calculateSeed)
import           Pos.Types                (Commitment (..), CommitmentsMap, FtsSeed (..),
                                           Opening (..), genCommitmentAndOpening,
                                           secretToFtsSeed, xorFtsSeed)
import           Pos.Util                 (nonrepeating, sublistN)

spec :: Spec
spec = describe "FollowTheSatoshi" $ do
    -- note that we can't make max size larger than 50 without changing it in
    -- Test.Pos.Util as well
    let smaller = modifyMaxSize (const 40) . modifyMaxSuccess (const 30)
    describe "calculateSeed" $ smaller $ do
        prop
            "finds the seed when all openings are present" $
            do n <- sized $ \size -> choose (1, max size 1)
               return $ recoverSecretsProp n n 0 0
        prop
            "finds the seed when all shares are present" $
            do n <- sized $ \size -> choose (1, max size 1)
               return $ recoverSecretsProp n 0 n 0
        prop
            "finds the seed when all secrets can be recovered" $
            do n <- sized $ \size -> choose (1, max size 1)
               n_overlap <- choose (0, n)
               n_openings <- choose (n_overlap, n)
               let n_shares = n - n_openings + n_overlap
               return $ recoverSecretsProp n n_openings n_shares n_overlap
        prop
            "fails to find the seed when some secrets can't be recovered" $
            do n <- sized $ \size -> choose (1, max size 1)
               n_overlap <- choose (0, n-1)
               n_openings <- choose (n_overlap, n-1)
               n_shares <- choose (n_overlap, n - n_openings + n_overlap - 1)
               -- to succeed, it must be that
               -- n_openings + n_shares - n_overlap >= n
               return $ recoverSecretsProp n n_openings n_shares n_overlap
        prop "secret recovering works" pending

----------------------------------------------------------------------------
-- Properties
----------------------------------------------------------------------------

-- | When each party has provided either an opening or shares (or both), we
-- should be able to recover the secret. When at least somebody hasn't
-- provided an opening or a secret, we should fail.
--
-- Specifically, we simulate the following scenario:
--
-- * Everybody has sent a commitment and generated a seed.
-- * 'n_openings' nodes have sent openings to the blockchain.
-- * 'n_shares' nodes have sent their shares to other nodes.
-- * /Among those,/ 'n_overlap' nodes have sent both.
-- * All nodes have sent -shares they have received- to the blockchain.
--   'n' shares are required to recover a secret.
recoverSecretsProp
    :: Int         -- ^ Number of parties
    -> Int         -- ^ How many have sent an opening
    -> Int         -- ^ How many have sent shares
    -> Int         -- ^ How many have sent both (the “overlap” parameter)
    -> Property
recoverSecretsProp n n_openings n_shares n_overlap
    | any (< 0) [n, n_openings, n_shares, n_overlap] = panic "negative"
    | n_overlap > n_openings = panic "n_overlap > n_openings"
    | n_overlap > n_shares   = panic "n_overlap > n_shares"
    | n_openings > n         = panic "n_openings > n"
    | n_shares > n           = panic "n_shares > n"
    -- there's a lower bound for the overlap, too (e.g. n=3,
    -- openings=2, shares=2, then overlap must be at least 1)
    | n - n_openings - n_shares + n_overlap < 0 = panic "overlap condition"

recoverSecretsProp n n_openings n_shares n_overlap = ioProperty $ do
    let threshold = pickThreshold n
    (keys, vssKeys, comms, opens) <- generateKeysAndMpc threshold n
    let seeds :: [FtsSeed]
        seeds = map (secretToFtsSeed . getOpening) opens
    let expectedSharedSeed :: FtsSeed
        expectedSharedSeed = foldl1' xorFtsSeed seeds
    haveSentBoth <- generate $
        sublistN n_overlap keys
    haveSentOpening <- generate $
        (haveSentBoth ++) <$>
        sublistN (n_openings - n_overlap) (keys \\ haveSentBoth)
    haveSentShares <- generate $
        (haveSentBoth ++) <$>
        sublistN (n_shares - n_overlap) (keys \\ haveSentOpening)
    let commitmentsMap = mkCommitmentsMap keys comms
    let openingsMap = HM.fromList
            [(getPub k, o)
              | (k, o) <- zip keys opens
              , k `elem` haveSentOpening]
    -- @generatedShares ! X@ = shares that X generated and sent to others
    -- generatedShares :: HashMap PublicKey (HashMap PublicKey Share)
    generatedShares <- do
        let sentShares (kp, _) = kp `elem` haveSentShares
        fmap HM.fromList $ forM (filter sentShares (zip keys comms)) $
            \(kp, comm) -> do
                let KeyPair pk _ = kp
                decShares <- getDecryptedShares vssKeys comm
                return (pk, HM.fromList (zip (map getPub keys) decShares))
    -- @sharesMap ! X@ = shares that X received from others
    let sharesMap = HM.fromList $ do
             KeyPair pk _ <- keys
             let receivedShares = HM.fromList $ do
                     (sender, senderShares) <- HM.toList generatedShares
                     case HM.lookup pk senderShares of
                         Nothing -> []
                         Just s  -> return (sender, s)
             return (pk, receivedShares)

    let shouldSucceed = n_openings + n_shares - n_overlap >= n
    let result = calculateSeed threshold commitmentsMap openingsMap sharesMap
    let debugInfo = sformat ("n = "%int%", n_openings = "%int%", "%
                             "n_shares = "%int%", n_overlap = "%int%
                             "\n"%
                             "these keys have sent openings:\n"%
                             "  "%listJson%"\n"%
                             "these keys have sent shares they got:\n"%
                             "  "%listJson)
                        n n_openings n_shares n_overlap
                        (map getPub haveSentOpening)
                        (map getPub haveSentShares)
    return $ counterexample (toS debugInfo) $ case (shouldSucceed, result) of
        -- we were supposed to find the seed
        (True, Right sharedSeed) ->
            sharedSeed === expectedSharedSeed
        (True, Left ftsErr) ->
            let err = sformat ("calculateSeed didn't find the seed (but "%
                               "should've) and failed with error:\n"%
                               "  "%build)
                              ftsErr
            in counterexample (toS err) failed
        -- we weren't supposed to find the seed
        (False, Left (NoSecretFound _)) ->
            property succeeded
        (False, Left ftsErr) ->
            let err = sformat ("calculateSeed failed with error "%build%" "%
                               "instead of NoSecretFound")
                              ftsErr
            in counterexample (toS err) failed
        (False, Right sharedSeed) ->
            let err = sformat ("calculateSeed succeeded, "%
                               "even though it couldn't\n"%
                               "  found seed: "%build%"\n"%
                               "  right seed: "%build)
                              sharedSeed expectedSharedSeed
            in counterexample (toS err <> "\n\n" <> show (n, threshold) <> "\n\n" <> show commitmentsMap <> "\n\n" <> show openingsMap <> "\n\n" <> show sharesMap) failed

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

generateKeysAndMpc
    :: Threshold
    -> Int
    -> IO ([KeyPair], [VssKeyPair], [Commitment], [Opening])
-- genCommitmentAndOpening fails on 0
generateKeysAndMpc _         0 = return ([], [], [], [])
generateKeysAndMpc threshold n = do
    keys           <- generate $ nonrepeating n
    vssKeys        <- generate $ nonrepeating n
    let vssPubKeys = map toVssPublicKey vssKeys
    (comms, opens) <-
        unzip <$> replicateM n (genCommitmentAndOpening threshold vssPubKeys)
    return (keys, vssKeys, comms, opens)

mkCommitmentsMap :: [KeyPair] -> [Commitment] -> CommitmentsMap
mkCommitmentsMap keys comms =
    HM.fromList $ do
        (KeyPair pk sk, comm) <- zip keys comms
        let epochIdx = 0  -- we don't care here
        let sig = sign sk (epochIdx, comm)
        return (pk, (comm, sig))

getDecryptedShares
    :: MonadRandom m
    => [VssKeyPair] -> Commitment -> m [Share]
getDecryptedShares vssKeys comm =
    forM (HM.toList (commShares comm)) $ \(pubKey, encShare) -> do
        let secKey = case find ((== pubKey) . toVssPublicKey) vssKeys of
                Just k  -> k
                Nothing -> panic $
                    sformat ("getDecryptedShares: counldn't \
                             \find key "%build) pubKey
        decryptShare secKey encShare

pickThreshold :: Int -> Threshold
pickThreshold n = fromIntegral (n `div` 2 + n `mod` 2)
