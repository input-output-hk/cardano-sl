{-# LANGUAGE DeriveGeneric #-}

-- | Dummy implementation of VSS.

module Pos.Crypto.SecretSharing
       ( Share
       , SecretProof
       , shareSecret
       , recoverSecret
       , verifyProof
       ) where


import           Data.Binary         (Binary)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, int, (%))
import           Universum

data Share = Share
    { share       :: ByteString
    , shareIndex  :: Int
    , totalShares :: Int
    , minNeeded   :: Int
    } deriving (Eq, Ord, Show, Generic)

instance Binary Share

newtype SecretProof =
    SecretProof ByteString
    deriving (Show, Eq, Generic)

instance Buildable.Buildable Share where
    build Share{..} = bprint ("share "%int%"/"%int) shareIndex totalShares

shareSecret
    :: Int              -- ^ How many parts to create
    -> Int              -- ^ How many parts should be enough
    -> ByteString
    -> (SecretProof, [Share])
shareSecret n k s = (SecretProof s, [Share s i n k | i <- [0..n-1]])

recoverSecret :: [Share] -> Maybe ByteString
recoverSecret [] = Nothing
recoverSecret (x:xs) = do
    guard (all (== share x) (map share xs))
    guard (length (ordNub (map shareIndex (x:xs))) >= minNeeded x)
    return (share x)

verifyProof :: SecretProof -> ByteString -> Bool
verifyProof (SecretProof p) s = p == s
