module Exporer.View.Types where

import Data.Maybe (Maybe)
import Data.Time.NominalDiffTime (NominalDiffTime)
import Data.Tuple (Tuple)
import Pos.Core.Types (Coin)
import Pos.Explorer.Web.ClientTypes (CAddress, CTxId)

-- put all view related types here in this file to generate lenses from it

newtype TxHeaderViewProps = TxHeaderViewProps
  { txhHash :: CTxId
  , txhTimeIssued :: Maybe NominalDiffTime
  , txhAmount :: Coin
  }


newtype TxBodyViewProps = TxBodyViewProps
  { txbInputs :: Array (Tuple CAddress Coin)
  , txbOutputs :: Array (Tuple CAddress Coin)
  , txbAmount :: Coin
  }
