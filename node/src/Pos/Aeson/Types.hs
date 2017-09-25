module Pos.Aeson.Types
       (
       ) where

import           Data.Aeson       (ToJSON (toJSON), object, (.=))
import           Data.Aeson.TH    (defaultOptions, deriveToJSON)
import           Formatting       (build, sformat)

import           Pos.Aeson.Crypto ()
import           Pos.Txp          (TxOut (..))
import           Pos.Types        (coinToInteger)
import           Pos.Web.Types    (GodTossingStage)

instance ToJSON TxOut where
    toJSON TxOut{..} = object [
        "coin"    .= coinToInteger txOutValue,
        "address" .= sformat build txOutAddress ]

-- NOTE: some of these types are used on frontend (PureScript).
-- We are automatically deriving instances there and they are
-- compitable now with `deriveToJSON defaultOptions ''Y`.
-- If datatype is used on frontend, please use this instead of
-- any other way of deriving if possible.

deriveToJSON defaultOptions ''GodTossingStage
