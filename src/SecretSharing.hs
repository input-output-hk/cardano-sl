module SecretSharing
       ( Share
       , shareSecret
       , recoverSecret
       ) where


import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, int, (%))
import           Protolude           hiding ((%))


data Share = Share {
    share       :: ByteString,
    shareIndex  :: Int,
    totalShares :: Int,
    minNeeded   :: Int }
  deriving (Eq, Ord, Show)

instance Buildable.Buildable Share where
    build Share{..} = bprint ("share "%int%"/"%int) shareIndex totalShares

shareSecret
    :: Int              -- ^ How many parts to create
    -> Int              -- ^ How many parts should be enough
    -> ByteString
    -> [Share]
shareSecret n k s = [Share s i n k | i <- [0..n-1]]

recoverSecret :: [Share] -> Maybe ByteString
recoverSecret [] = Nothing
recoverSecret (x:xs) = do
    guard (all (== share x) (map share xs))
    guard (length (ordNub (map shareIndex (x:xs))) >= minNeeded x)
    return (share x)
