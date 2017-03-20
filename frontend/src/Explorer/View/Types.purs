module Exporer.View.Types where

import Data.Maybe (Maybe)
import Data.Time.NominalDiffTime (NominalDiffTime)
import Pos.Core.Types (Coin)
import Pos.Explorer.Web.ClientTypes (CTxId)


newtype TxHeaderViewProps = TxHeaderViewProps
  { txhHash :: CTxId
  , txhTimeIssued :: Maybe NominalDiffTime
  , txhAmount :: Coin
  }
