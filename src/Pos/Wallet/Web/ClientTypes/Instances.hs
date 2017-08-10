-- | Instances for client types

module Pos.Wallet.Web.ClientTypes.Instances () where

import           Universum

import           Servant.API                      (FromHttpApiData (..))

import           Pos.Core                         (Address, Coin, decodeTextAddress,
                                                   mkCoin)
import           Pos.Wallet.Web.ClientTypes.Types (CAccountId (..), CId (..),
                                                   CPassPhrase (..), CTxId, addressToCId,
                                                   mkCTxId)

instance FromHttpApiData Coin where
    parseUrlPiece = fmap mkCoin . parseUrlPiece

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData (CId w) where
    parseUrlPiece = fmap addressToCId . decodeTextAddress

instance FromHttpApiData CAccountId where
    parseUrlPiece = fmap CAccountId . parseUrlPiece

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking whether received Text is really valid CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CPassPhrase where
    parseUrlPiece = pure . CPassPhrase
