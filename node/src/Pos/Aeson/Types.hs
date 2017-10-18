module Pos.Aeson.Types
       (
       ) where

import           Universum

import           Data.Aeson       (FromJSON (..), ToJSON (toJSON), object, withObject,
                                   (.:), (.=))
import           Data.Aeson.TH    (defaultOptions, deriveToJSON)
import           Formatting       (build, sformat)

import           Pos.Aeson.Crypto ()
import           Pos.Core.Address (decodeTextAddress)
import           Pos.Txp          (TxOut (..))
import           Pos.Types        (coinToInteger, integerToCoin)
import           Pos.Util.Util    (eitherToFail)
import           Pos.Web.Types    (GodTossingStage)

instance ToJSON TxOut where
    toJSON TxOut{..} = object [
        "coin"    .= coinToInteger txOutValue,
        "address" .= sformat build txOutAddress ]


instance FromJSON TxOut where
    parseJSON = withObject "TxOut" $ \o -> do
        txOutValue   <- eitherToFail . integerToCoin =<< o .: "coin"
        txOutAddress <- eitherToFail . decodeTextAddress =<< o .: "address"
        return $ TxOut {..}

-- NOTE: some of these types are used on frontend (PureScript).
-- We are automatically deriving instances there and they are
-- compitable now with `deriveToJSON defaultOptions ''Y`.
-- If datatype is used on frontend, please use this instead of
-- any other way of deriving if possible.

deriveToJSON defaultOptions ''GodTossingStage
