module Pos.Core.Common.AddrStakeDistribution
       ( AddrStakeDistribution (..)

       , mkMultiKeyDistr
       , MultiKeyDistrError (..)
       ) where

import           Universum hiding (id)

import           Control.Exception.Safe (Exception (displayException))
import           Control.Lens (_Left)
import           Control.Monad.Except (MonadError (throwError))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util (mapJson)

import           Pos.Binary.Class (Bi (..), szCases)
import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Hashing (shortHashF)
import           Pos.Util.Util (cborError, toCborError)

import           Pos.Core.Common.CoinPortion
import           Pos.Core.Common.StakeholderId

-- | Stake distribution associated with an address.
data AddrStakeDistribution
    = BootstrapEraDistr
    -- ^ Stake distribution for bootstrap era.
    | SingleKeyDistr !StakeholderId
    -- ^ Stake distribution stating that all stake should go to the given stakeholder.
    | UnsafeMultiKeyDistr !(Map StakeholderId CoinPortion)
    -- ^ Stake distribution which gives stake to multiple
    -- stakeholders. 'CoinPortion' is a portion of an output (output
    -- has a value, portion of this value is stake). The constructor
    -- is unsafe because there are some predicates which must hold:
    --
    -- • the sum of portions must be @maxBound@ (basically 1);
    -- • all portions must be positive;
    -- • there must be at least 2 items, because if there is only one item,
    -- 'SingleKeyDistr' can be used instead (which is smaller).
    deriving (Eq, Ord, Show, Generic, Typeable)

instance Buildable AddrStakeDistribution where
    build =
        \case
            BootstrapEraDistr -> "Bootstrap era distribution"
            SingleKeyDistr id ->
                bprint ("Single key distribution ("%shortHashF%")") id
            UnsafeMultiKeyDistr distr ->
                bprint ("Multi key distribution: "%mapJson) distr

instance NFData AddrStakeDistribution

instance Bi AddrStakeDistribution where
    encode =
        \case
            BootstrapEraDistr -> Bi.encodeListLen 0
            SingleKeyDistr id -> encode (0 :: Word8, id)
            UnsafeMultiKeyDistr distr -> encode (1 :: Word8, distr)
    decode =
        Bi.decodeListLenCanonical >>= \case
            0 -> pure BootstrapEraDistr
            2 ->
                decode @Word8 >>= \case
                    0 -> SingleKeyDistr <$> decode
                    1 -> toCborError . (_Left %~ toText . displayException) .
                         mkMultiKeyDistr =<< decode
                    tag -> cborError $
                        "decode @AddrStakeDistribution: unexpected tag " <>
                        pretty tag
            len -> cborError $
                "decode @AddrStakeDistribution: unexpected length " <> pretty len
    encodedSizeExpr size _ = szCases
        [ Bi.Case "BoostrapEraDistr" 1
        , let SingleKeyDistr id = error "unused"
          in  Bi.Case "SingleKeyDistr" $ size ((,) <$> pure (0 :: Word8) <*> pure id)
        , let UnsafeMultiKeyDistr distr = error "unused"
          in  Bi.Case "UnsafeMultiKeyDistr" $ size ((,) <$> pure (1 :: Word8) <*> pure distr)
        ]

data MultiKeyDistrError
    = MkdMapIsEmpty
    | MkdMapIsSingleton
    | MkdNegativePortion
    | MkdSumNot1
    deriving (Show)

instance Buildable MultiKeyDistrError where
    build = mappend "mkMultiKeyDistr: " . \case
        MkdMapIsEmpty -> "map is empty"
        MkdMapIsSingleton -> "map's size is 1, use SingleKeyDistr"
        MkdNegativePortion -> "all portions must be positive"
        MkdSumNot1 -> "distributions' sum must be equal to 1"

instance Exception MultiKeyDistrError where
    displayException = toString . pretty

-- | Safe constructor of multi-key distribution. It checks invariants
-- of this distribution and returns an error if something is violated.
mkMultiKeyDistr ::
       MonadError MultiKeyDistrError m
    => Map StakeholderId CoinPortion
    -> m AddrStakeDistribution
mkMultiKeyDistr distrMap = UnsafeMultiKeyDistr distrMap <$ check
  where
    check = do
        when (null distrMap) $ throwError MkdMapIsEmpty
        when (length distrMap == 1) $ throwError MkdMapIsSingleton
        unless (all ((> 0) . getCoinPortion) distrMap) $
            throwError MkdNegativePortion
        let distrSum = sum $ map getCoinPortion distrMap
        unless (distrSum == coinPortionDenominator) $
            throwError MkdSumNot1

deriveSafeCopySimple 0 'base ''AddrStakeDistribution
