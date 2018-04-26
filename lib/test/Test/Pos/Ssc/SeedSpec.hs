-- | Specification of Pos.calculateSeed

module Test.Pos.Ssc.SeedSpec
       ( spec
       ) where

import           Universum

import           Control.Lens (each, traverseOf)
import           Crypto.Random (MonadRandom)
import qualified Data.HashMap.Strict as HM
import           Data.List (lookup, unzip, (\\))
import qualified Data.List.NonEmpty as NE
import           Formatting (build, int, sformat, shown, (%))
import           Serokell.Util (listJson)
import           Test.Hspec (Spec, describe, pending)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, choose, counterexample, generate, ioProperty, property,
                                  sized, (===))
import           Test.QuickCheck.Property (failed, succeeded)

import           Pos.Binary
import           Pos.Core (AddressHash, HasConfiguration, SharedSeed (..), StakeholderId,
                           addressHash, mkCoin)
import           Pos.Core.Ssc (Commitment (..), CommitmentsMap, Opening (..), getCommShares,
                               getCommitmentsMap, mkCommitmentsMap)
import           Pos.Crypto (DecShare, PublicKey, SecretKey, SignTag (SignCommitment), Threshold,
                             VssKeyPair, VssPublicKey, decryptShare, sign, toPublic, toVssPublicKey,
                             protocolMagic)
import           Pos.Ssc (SscSeedError (..), calculateSeed, genCommitmentAndOpening,
                          secretToSharedSeed, vssThreshold)
import           Pos.Util (nonrepeating, sublistN)

import           Test.Pos.Configuration (withDefConfiguration)

getPubAddr :: SecretKey -> AddressHash PublicKey
getPubAddr = addressHash . toPublic

spec :: Spec
spec = withDefConfiguration $ do
    -- note that we can't make max size larger than 50 without changing it in
    -- Test.Pos.Configuration as well
    let smaller = modifyMaxSize (const 40) . modifyMaxSuccess (const 30)
    describe "calculateSeed" $ smaller $ do
        prop
            "finds the seed when all openings are present" $
            do n <- sized $ \size -> choose (4, max size 4)
               return $ recoverSecretsProp n n 0 0
        prop
            "finds the seed when all shares are present" $
            do n <- sized $ \size -> choose (4, max size 4)
               return $ recoverSecretsProp n 0 n 0
        prop
            "finds the seed when all secrets can be recovered" $
            do n <- sized $ \size -> choose (4, max size 4)
               n_overlap <- choose (0, n)
               n_openings <- choose (n_overlap, n)
               let n_shares = n - n_openings + n_overlap
               return $ recoverSecretsProp n n_openings n_shares n_overlap
        -- [CSL-50]: we are in process of thinking about this property
        -- prop
        --     "fails to find the seed when some secrets can't be recovered" $
        --     do n <- sized $ \size -> choose (4, max size 4)
        --        n_overlap <- choose (0, n-1)
        --        n_openings <- choose (n_overlap, n-1)
        --        n_shares <- choose (n_overlap, n - n_openings + n_overlap - 1)
        --        -- to succeed, it must be that
        --        -- n_openings + n_shares - n_overlap >= n
        --        return $ recoverSecretsProp n n_openings n_shares n_overlap
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
    :: HasConfiguration
    => Int         -- ^ Number of parties
    -> Int         -- ^ How many have sent an opening
    -> Int         -- ^ How many have sent shares
    -> Int         -- ^ How many have sent both (the “overlap” parameter)
    -> Property
recoverSecretsProp n n_openings n_shares n_overlap
    | any (< 0) [n, n_openings, n_shares, n_overlap] =
          error "recoverSecretsProp: negative"
    | n < 4 =
          error "recoverSecretsProp: n < 4"
    | n_overlap > n_openings =
          error "recoverSecretsProp: n_overlap > n_openings"
    | n_overlap > n_shares =
          error "recoverSecretsProp: n_overlap > n_shares"
    | n_openings > n =
          error "recoverSecretsProp: n_openings > n"
    | n_shares > n =
          error "recoverSecretsProp: n_shares > n"
    -- there's a lower bound for the overlap, too (e.g. n=3,
    -- openings=2, shares=2, then overlap must be at least 1)
    | n - n_openings - n_shares + n_overlap < 0 =
          error "recoverSecretsProp: overlap condition"

recoverSecretsProp n n_openings n_shares n_overlap = ioProperty $ do
    let threshold = vssThreshold n
    (keys', vssKeys, comms, opens) <- generateKeysAndMpc threshold n
    let des = fromBinary
        seeds :: [SharedSeed]
        seeds = rights $ map (fmap secretToSharedSeed . des . getOpening) opens
    let expectedSharedSeed :: SharedSeed
        expectedSharedSeed = mconcat seeds
    haveSentBoth <- generate $
        sublistN n_overlap keys'
    haveSentOpening <- generate $
        (haveSentBoth ++) <$>
        sublistN (n_openings - n_overlap) (keys' \\ haveSentBoth)
    haveSentShares <- generate $
        (haveSentBoth ++) <$>
        sublistN (n_shares - n_overlap) (keys' \\ haveSentOpening)
    let commitmentsMap = mkCommitmentsMap' keys' comms
    let vssMap = mkVssMap keys' vssKeys
    let richmen = getCommitmentsMap commitmentsMap & each .~ mkCoin 1000
    let openingsMap = HM.fromList
            [(getPubAddr k, o)
              | (k, o) <- zip keys' opens
              , k `elem` haveSentOpening]
    -- @generatedShares ! X@ = shares that X generated and sent to others
    -- generatedShares :: HashMap PublicKey (HashMap PublicKey [DecShare])
    generatedShares <- do
        let sentShares (kp, _) = kp `elem` haveSentShares
        fmap HM.fromList $ forM (filter sentShares (zip keys' comms)) $
            \(kp, comm) -> do
                let addr = getPubAddr kp
                let findPub vss = getPubAddr <$>
                                  lookup vss (zip (toList vssKeys) keys')
                decShares <- getDecryptedShares vssKeys comm
                let decShares' = mapMaybe (traverseOf _1 findPub) decShares
                return (addr, HM.fromList decShares')
    -- @sharesMap ! X@ = shares that X received from others
    let sharesMap = HM.fromList $ do
             addr <- getPubAddr <$> keys'
             let ser = asBinary @DecShare
                 receivedShares = HM.fromList $ do
                     (sender, senderShares) <- HM.toList generatedShares
                     maybe [] one $ do
                         s <- HM.lookup addr senderShares
                         (sender,) <$> nonEmpty (map ser s)
             return (addr, receivedShares)

    let shouldSucceed = n_openings + n_shares - n_overlap >= n
    let result = calculateSeed commitmentsMap vssMap
                               openingsMap sharesMap
                               richmen
    let debugInfo = sformat ("n = "%int%", n_openings = "%int%", "%
                             "n_shares = "%int%", n_overlap = "%int%
                             "\n"%
                             "these keys' have sent openings:\n"%
                             "  "%listJson%"\n"%
                             "these keys' have sent shares they got:\n"%
                             "  "%listJson)
                        n n_openings n_shares n_overlap
                        (map toPublic haveSentOpening)
                        (map toPublic haveSentShares)
    return $ counterexample (toString debugInfo) $ case (shouldSucceed, result) of
        -- we were supposed to find the seed
        (True, Right sharedSeed) ->
            sharedSeed === expectedSharedSeed
        (True, Left ftsErr) ->
            let err = sformat ("calculateSeed didn't find the seed (but "%
                               "should've) and failed with error:\n"%
                               "  "%build)
                              ftsErr
            in counterexample (toString err) failed
        -- we weren't supposed to find the seed
        (False, Left (NoSecretFound _)) ->
            property succeeded
        (False, Left ftsErr) ->
            let err = sformat ("calculateSeed failed with error "%build%" "%
                               "instead of NoSecretFound")
                              ftsErr
            in counterexample (toString err) failed
        (False, Right sharedSeed) ->
            let err = sformat ("calculateSeed succeeded, "%
                               "even though it couldn't\n"%
                               "  found seed: "%build%"\n"%
                               "  right seed: "%build)
                              sharedSeed expectedSharedSeed
            in counterexample (toString err <> "\n\n" <> show (n, threshold) <> "\n\n" <> show commitmentsMap <> "\n\n" <> show openingsMap <> "\n\n" <> show sharesMap) failed

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

generateKeysAndMpc
    :: Threshold
    -> Int
    -> IO ([SecretKey], NonEmpty VssKeyPair, [Commitment], [Opening])
generateKeysAndMpc _         0 = error "generateKeysAndMpc: 0 is passed"
generateKeysAndMpc threshold n = do
    keys'           <- generate $ nonrepeating n
    vssKeys        <- sortWith toVssPublicKey <$> generate (nonrepeating n)
    let lvssPubKeys = NE.fromList $ map toVssPublicKey vssKeys
    (comms, opens) <-
        unzip <$> replicateM n (genCommitmentAndOpening threshold lvssPubKeys)
    return (keys', NE.fromList vssKeys, comms, opens)

mkCommitmentsMap' :: HasConfiguration => [SecretKey] -> [Commitment] -> CommitmentsMap
mkCommitmentsMap' keys' comms =
    mkCommitmentsMap $ do
        (sk, comm) <- zip keys' comms
        let epochIdx = 0  -- we don't care here
        let sig = sign protocolMagic SignCommitment sk (epochIdx, comm)
        return (toPublic sk, comm, sig)

mkVssMap :: [SecretKey]
         -> NonEmpty VssKeyPair
         -> HashMap StakeholderId (AsBinary VssPublicKey)
mkVssMap keys' vssKeys =
    HM.fromList $
        zip (map getPubAddr keys')
            (map (asBinary . toVssPublicKey) (toList vssKeys))

getDecryptedShares
    :: MonadRandom m
    => NonEmpty VssKeyPair -> Commitment -> m [(VssKeyPair, [DecShare])]
getDecryptedShares vssKeys comm = do
    let shares = fromMaybe (error "getDecryptedShares: can't decode shares")
                           (getCommShares comm)
    forM shares $ \(pubKey, encShares) -> do
        let secKey = case find ((== pubKey) . toVssPublicKey) vssKeys of
                Just k  -> k
                Nothing -> error $
                    sformat ("getDecryptedShares: counldn't \
                             \find key "%shown) pubKey
        (secKey,) . toList <$> mapM (decryptShare secKey) encShares
