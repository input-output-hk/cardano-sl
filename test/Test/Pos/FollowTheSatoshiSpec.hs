{-# LANGUAGE ScopedTypeVariables #-}

-- | Specification of Pos.FollowTheSatoshi

module Test.Pos.FollowTheSatoshiSpec
       ( spec
       ) where

import qualified Data.ByteString       as BS
import qualified Data.HashMap.Strict   as HM
import           Data.List             (foldl1')
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, choose, forAll, property, (===))
import           Universum

import           Pos.Crypto            (mkSigned)
import           Pos.FollowTheSatoshi  (calculateSeed)
import           Pos.Types             (Commitment (..), FtsSeed (..), Opening (..),
                                        shareFtsSeed)
import           Test.Pos.Util         (KeyPair (..), VssKeyPair (..), nonrepeating)

spec :: Spec
spec = describe "FollowTheSatoshi" $ do
    describe "calculateSeed" $ do
        prop
            "calculates seed correctly when all openings are present" $
            (forAll (choose (1,40)) allOpeningsPresentProp)

allOpeningsPresentProp :: Word -> Property
allOpeningsPresentProp n = property $ do
    keys    :: [KeyPair]    <- nonrepeating (fromIntegral n)
    vssKeys :: [VssKeyPair] <- nonrepeating (fromIntegral n)
    seeds   :: [FtsSeed]    <- nonrepeating (fromIntegral n)
    let vssPubKeys = map getVssPub vssKeys
    let commitments = HM.fromList $ do
            (KeyPair pk sk, seed) <- zip keys seeds
            let (proof, shares) = shareFtsSeed vssPubKeys n seed
            let comm = Commitment
                    { commProof  = proof
                    , commShares = HM.fromList (zip vssPubKeys shares)
                    }
            return (pk, mkSigned sk comm)
    let openings = HM.fromList $ do
            (KeyPair pk sk, seed) <- zip keys seeds
            return (pk, mkSigned sk (Opening seed))
    let shares = mempty
    return $ calculateSeed commitments openings shares
                 === Right (foldl1' xorSeed seeds)

xorSeed :: FtsSeed -> FtsSeed -> FtsSeed
xorSeed (FtsSeed a) (FtsSeed b) = FtsSeed $ BS.pack (BS.zipWith xor a b)

{-
calculateSeed
    :: CommitmentsMap        -- ^ All participating nodes
    -> OpeningsMap
    -> SharesMap
    -> Either FtsError FtsSeed

type CommitmentsMap = HashMap PublicKey (Signed Commitment)
type OpeningsMap = HashMap PublicKey (Signed Opening)
type SharesMap = HashMap PublicKey (Signed (HashMap PublicKey Share))

data Commitment = Commitment
    { commProof  :: !SecretProof
    , commShares :: !(HashMap VssPublicKey (EncShare))
    } deriving (Show, Eq, Generic)

newtype SecretProof =
    SecretProof Secret
    deriving (Show, Eq, Generic, Binary)

shareSecret
    :: [VssPublicKey]  -- ^ Public keys of parties
    -> Word            -- ^ How many parts should be enough
    -> Secret          -- ^ Secret to share
    -> (SecretProof, [EncShare])  -- ^ i-th share is encrypted using i-th key
shareSecret keys _ s = (SecretProof s, map mkShare keys)
  where
    mkShare key = encryptShare key (Share s)

newtype Opening = Opening
    { getOpening :: FtsSeed
    } deriving (Show, Eq, Generic, Binary, Buildable)

newtype Share = Share
    { getShare :: Secret
    } deriving (Eq, Ord, Show, Binary)

newtype Secret = Secret
    { getSecret :: ByteString
    } deriving (Show, Eq, Ord, Binary)
-}
