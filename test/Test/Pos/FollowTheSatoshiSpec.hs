{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specification of Pos.FollowTheSatoshi

module Test.Pos.FollowTheSatoshiSpec
       ( spec
       ) where

import qualified Data.ByteString          as BS
import qualified Data.HashMap.Strict      as HM
import           Data.List                (foldl1', (\\))
import           Formatting               (build, int, later, sformat, (%))
import           Serokell.Util            (listBuilderJSON)
import           Test.Hspec               (Spec, describe, pending)
import           Test.Hspec.QuickCheck    (modifyMaxSize, prop)
import           Test.QuickCheck          (Gen, Property, choose, counterexample,
                                           property, sized, (===))
import           Test.QuickCheck.Property (failed, succeeded)
import           Universum

import           Pos.Crypto               (PublicKey, Share, decryptShare, sign)
import           Pos.FollowTheSatoshi     (FtsError (..), calculateSeed)
import           Pos.Types                (Commitment (..), CommitmentsMap, FtsSeed (..),
                                           Opening (..), xorFtsSeed)
import           Test.Pos.Util            (KeyPair (..), VssKeyPair (..), nonrepeating,
                                           sublistN)

spec :: Spec
spec = describe "FollowTheSatoshi" $ do
    describe "calculateSeed" $ modifyMaxSize (const 40) $ do
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

recoverSecretsProp n n_openings n_shares n_overlap = property $ do
    (keys, vssKeys, seeds) <- generateKeysAndSeeds n
    let rightSeed :: FtsSeed
        rightSeed = foldl1' xorFtsSeed seeds
    haveSentBoth <-
        sublistN n_overlap keys
    haveSentOpening <-
        (haveSentBoth ++) <$>
        sublistN (n_openings - n_overlap) (keys \\ haveSentBoth)
    haveSentShares <-
        (haveSentBoth ++) <$>
        sublistN (n_shares - n_overlap) (keys \\ haveSentOpening)
    let commitments = mkCommitmentsMap keys vssKeys seeds
    let openings = HM.fromList $ do
            (KeyPair pk sk, seed) <- zip keys seeds
            guard (KeyPair pk sk `elem` haveSentOpening)
            return (pk, Opening seed)
    -- @generatedShares ! X@ = shares that X generated and sent to others
    let generatedShares :: HashMap PublicKey (HashMap PublicKey Share)
        generatedShares = HM.fromList $ do
            (KeyPair pk sk, seed) <- zip keys seeds
            guard (KeyPair pk sk `elem` haveSentShares)
            let vssPubKeys = map getVssPub vssKeys
                vssSecKeys = map getVssSec vssKeys
            let (_, shares) = shareFtsSeed vssPubKeys (fromIntegral n) seed
            let decryptedShares = zipWith decryptShare vssSecKeys shares
            return (pk, HM.fromList (zip (map getPub keys) decryptedShares))
    -- @shares ! X@ = shares that X received from others
    let shares = HM.fromList $ do
            KeyPair pk _ <- keys
            let receivedShares = HM.fromList $ do
                    (sender, senderShares) <- HM.toList generatedShares
                    case HM.lookup pk senderShares of
                        Nothing -> []
                        Just s  -> return (sender, s)
            return (pk, receivedShares)

    let shouldSucceed = n_openings + n_shares - n_overlap >= n
    let result = calculateSeed commitments openings shares
    let debugInfo = sformat ("n = "%int%", n_openings = "%int%", "%
                             "n_shares = "%int%", n_overlap = "%int%
                             "\n"%
                             "these keys have sent openings:\n"%
                             "  "%later listBuilderJSON%"\n"%
                             "these keys have sent shares they got:\n"%
                             "  "%later listBuilderJSON)
                        n n_openings n_shares n_overlap
                        (map getPub haveSentOpening)
                        (map getPub haveSentShares)
    return $ counterexample (toS debugInfo) $ case (shouldSucceed, result) of
        -- we were supposed to find the seed
        (True, Right foundSeed) ->
            foundSeed === rightSeed
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
        (False, Right foundSeed) ->
            let err = sformat ("calculateSeed succeeded, "%
                               "even though it couldn't\n"%
                               "  found seed: "%build%"\n"%
                               "  right seed: "%build)
                              foundSeed rightSeed
            in counterexample (toS err) failed

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

generateKeysAndSeeds :: Int -> Gen ([KeyPair], [VssKeyPair], [FtsSeed])
generateKeysAndSeeds n = do
    keys    <- nonrepeating n
    vssKeys <- nonrepeating n
    seeds   <- nonrepeating n
    return (keys, vssKeys, seeds)

mkCommitmentsMap :: [KeyPair] -> [VssKeyPair] -> [FtsSeed] -> CommitmentsMap
mkCommitmentsMap keys vssKeys seeds =
    HM.fromList $ do
        let vssPubKeys = map getVssPub vssKeys
        let n = fromIntegral (length vssKeys)
        (KeyPair pk sk, seed) <- zip keys seeds
        let (proof, shares) = shareFtsSeed vssPubKeys n seed
        let comm = Commitment
                { commProof  = proof
                , commShares = HM.fromList (zip vssPubKeys shares)
                }
        let epochIdx = 0  -- we don't care here
        let sig = sign sk (epochIdx, comm)
        return (pk, (comm, sig))
