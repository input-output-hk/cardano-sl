module Pos.Aeson.Types
       (
       ) where

import           Data.Aeson           (ToJSON (toJSON), object, (.=))
import           Data.Aeson.TH        (defaultOptions, deriveToJSON)
import           Formatting           (build, sformat)
import           Serokell.Util.Base16 (base16F)
import           Universum

import           Pos.Aeson.Crypto     ()
import           Pos.Data.Attributes  (Attributes (..))
import           Pos.Txp              (TxOut (..))
import           Pos.Types            (Address (..), ApplicationName (..),
                                       BlockCount (..), ChainDifficulty, Coin, EpochIndex,
                                       LocalSlotIndex, SharedSeed, SlotCount (..), SlotId,
                                       coinToInteger)
import           Pos.Web.Types        (GodTossingStage)

instance ToJSON SharedSeed where
    toJSON = toJSON . pretty

instance ToJSON a => ToJSON (Attributes a) where
    toJSON = toJSON . attrData

instance ToJSON Address where
    toJSON PubKeyAddress{..} = object [
        "tag" .= ("PubKeyAddress" :: Text),
        "addrKeyHash" .= addrKeyHash ]
    toJSON ScriptAddress{..} = object [
        "tag" .= ("ScriptAddress" :: Text),
        "addrScriptHash" .= addrScriptHash ]
    toJSON RedeemAddress{..} = object [
        "tag" .= ("RedeemAddress" :: Text),
        "addrRedeemKeyHash" .= addrRedeemKeyHash ]
    toJSON (UnknownAddressType t bs) = object [
        "tag" .= ("UnknownAddressType" :: Text),
        "type" .= t,
        "content" .= sformat base16F bs ]

instance ToJSON TxOut where
    toJSON TxOut{..} = object [
        "coin"    .= coinToInteger txOutValue,
        "address" .= sformat build txOutAddress ]

deriving instance ToJSON SlotCount

-- NOTE: some of these types are used on frontend (PureScript).
-- We are automatically deriving instances there and they are
-- compitable now with `deriveToJSON defaultOptions ''Y`.
-- If datatype is used on frontend, please use this instead of
-- any other way of deriving if possible.

deriveToJSON defaultOptions ''BlockCount
deriveToJSON defaultOptions ''ApplicationName
deriveToJSON defaultOptions ''ChainDifficulty
deriveToJSON defaultOptions ''SlotId
deriveToJSON defaultOptions ''LocalSlotIndex
deriveToJSON defaultOptions ''EpochIndex
deriveToJSON defaultOptions ''Coin
deriveToJSON defaultOptions ''GodTossingStage
