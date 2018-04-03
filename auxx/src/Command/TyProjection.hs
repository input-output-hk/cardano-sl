module Command.TyProjection
       ( tyValue
       , tyEither
       , tyAddress
       , tyPublicKey
       , tyTxOut
       , tyAddrStakeDistr
       , tyFilePath
       , tyInt
       , tyWord
       , tyWord32
       , tyByte
       , tySecond
       , tyBool
       , tyScriptVersion
       , tyCoin
       , tyCoinPortion
       , tyStakeholderId
       , tyAddrDistrPart
       , tyEpochIndex
       , tyHash
       , tyBlockVersion
       , tySoftwareVersion
       , tyBlockVersionModifier
       , tyProposeUpdateSystem
       , tySystemTag
       , tyApplicationName
       , tyString
       ) where

import           Universum

import           Data.Scientific (Scientific, floatingOrInteger, toBoundedInteger, toRealFloat)
import           Data.Time.Units (TimeUnit, Microsecond, convertUnit, fromMicroseconds)
import           Serokell.Data.Memory.Units (Byte, fromBytes)

import           Pos.Core (AddrStakeDistribution (..), Address, BlockVersion, Coin,
                           CoinPortion, EpochIndex, ScriptVersion, SoftwareVersion, StakeholderId,
                           mkCoin, unsafeCoinPortionFromDouble, unsafeGetCoin)
import           Pos.Core.Txp (TxOut (..))
import           Pos.Crypto (AHash (..), Hash, PublicKey)
import           Pos.Update (BlockVersionModifier (..), ApplicationName (..), SystemTag (..))

import           Lang.Argument (TyProjection (..), TypeName (..))
import           Lang.Value (AddrDistrPart (..), ProposeUpdateSystem (..),
                             Value (..), _ValueAddrDistrPart, _ValueAddrStakeDistribution,
                             _ValueAddress, _ValueBlockVersion, _ValueBlockVersionModifier,
                             _ValueBool, _ValueFilePath, _ValueHash, _ValueNumber,
                             _ValueProposeUpdateSystem, _ValuePublicKey,
                             _ValueSoftwareVersion, _ValueStakeholderId, _ValueString, _ValueTxOut)

tyValue :: TyProjection Value
tyValue = TyProjection "Value" Just

infixr `tyEither`

tyEither :: TyProjection a -> TyProjection b -> TyProjection (Either a b)
tyEither tpa tpb = TyProjection
    { tpTypeName = TypeNameEither (tpTypeName tpa) (tpTypeName tpb)
    , tpMatcher = \v ->
        Left <$> tpMatcher tpa v <|>
        Right <$> tpMatcher tpb v
    }

tyAddress :: TyProjection Address
tyAddress = TyProjection "Address" (preview _ValueAddress)

tyPublicKey :: TyProjection PublicKey
tyPublicKey = TyProjection "PublicKey" (preview _ValuePublicKey)

tyTxOut :: TyProjection TxOut
tyTxOut = TyProjection "TxOut" (preview _ValueTxOut)

tyAddrStakeDistr :: TyProjection AddrStakeDistribution
tyAddrStakeDistr = TyProjection "AddrStakeDistribution" (preview _ValueAddrStakeDistribution)

tyFilePath :: TyProjection FilePath
tyFilePath = TyProjection "FilePath" (preview _ValueFilePath)

tyInt :: TyProjection Int
tyInt = TyProjection "Int" (toBoundedInteger <=< preview _ValueNumber)

tyWord :: TyProjection Word
tyWord = TyProjection "Word" (toBoundedInteger <=< preview _ValueNumber)

tyWord32 :: TyProjection Word32
tyWord32 = TyProjection "Word32" (toBoundedInteger <=< preview _ValueNumber)

tyByte :: TyProjection Byte
tyByte = fromBytes <$> TyProjection "Byte" (sciToInteger <=< preview _ValueNumber)

sciToInteger :: Scientific -> Maybe Integer
sciToInteger = either (const Nothing) Just . floatingOrInteger @Double @Integer

tySecond :: forall a . TimeUnit a => TyProjection a
tySecond =
    convertUnit . (fromMicroseconds . fromIntegral . (*) 1000000 :: Int -> Microsecond) <$>
    TyProjection "Second" (toBoundedInteger <=< preview _ValueNumber)

tyScriptVersion :: TyProjection ScriptVersion
tyScriptVersion = TyProjection "ScriptVersion" (toBoundedInteger <=< preview _ValueNumber)

tyBool :: TyProjection Bool
tyBool = TyProjection "Bool" (preview _ValueBool)

-- | Small hack to use 'toBoundedInteger' for 'Coin'.
newtype PreCoin = PreCoin { getPreCoin :: Word64 }
    deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Bounded PreCoin where
    minBound = PreCoin . unsafeGetCoin $ minBound
    maxBound = PreCoin . unsafeGetCoin $ maxBound

fromPreCoin :: PreCoin -> Coin
fromPreCoin = mkCoin . getPreCoin

tyCoin :: TyProjection Coin
tyCoin = fromPreCoin <$> TyProjection "Coin" (toBoundedInteger <=< preview _ValueNumber)

coinPortionFromDouble :: Double -> Maybe CoinPortion
coinPortionFromDouble a
    | a >= 0, a <= 1 = Just $ unsafeCoinPortionFromDouble a
    | otherwise = Nothing

tyCoinPortion :: TyProjection CoinPortion
tyCoinPortion = TyProjection "CoinPortion" (coinPortionFromDouble . toRealFloat <=< preview _ValueNumber)

tyStakeholderId :: TyProjection StakeholderId
tyStakeholderId = TyProjection "StakeholderId" (preview _ValueStakeholderId)

tyAddrDistrPart :: TyProjection AddrDistrPart
tyAddrDistrPart = TyProjection "AddrDistrPart" (preview _ValueAddrDistrPart)

tyEpochIndex :: TyProjection EpochIndex
tyEpochIndex = TyProjection "EpochIndex" (toBoundedInteger <=< preview _ValueNumber)

tyHash :: TyProjection (Hash a)
tyHash = getAHash <$> TyProjection "Hash" (preview _ValueHash)

tyBlockVersion :: TyProjection BlockVersion
tyBlockVersion = TyProjection "BlockVersion" (preview _ValueBlockVersion)

tySoftwareVersion :: TyProjection SoftwareVersion
tySoftwareVersion = TyProjection "SoftwareVersion" (preview _ValueSoftwareVersion)

tyBlockVersionModifier :: TyProjection BlockVersionModifier
tyBlockVersionModifier = TyProjection "BlockVersionModifier" (preview _ValueBlockVersionModifier)

tyProposeUpdateSystem :: TyProjection ProposeUpdateSystem
tyProposeUpdateSystem = TyProjection "ProposeUpdateSystem" (preview _ValueProposeUpdateSystem)

tySystemTag :: TyProjection SystemTag
tySystemTag = TyProjection "SystemTag" ((fmap . fmap) (SystemTag . fromString) (preview _ValueString))

tyApplicationName :: TyProjection ApplicationName
tyApplicationName = TyProjection "ApplicationName" ((fmap . fmap) (ApplicationName . fromString) (preview _ValueString))

tyString :: TyProjection String
tyString = TyProjection "String" (preview _ValueString)
