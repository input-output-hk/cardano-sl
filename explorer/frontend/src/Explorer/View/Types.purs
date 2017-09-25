module Exporer.View.Types where

import Data.Maybe (Maybe)
import Data.Time.NominalDiffTime (NominalDiffTime)
import Data.Tuple (Tuple)
import Pos.Explorer.Web.ClientTypes (CCoin, CAddress, CTxId)

-- put all view related types here in this file to generate lenses from it

newtype TxHeaderViewProps = TxHeaderViewProps
  { txhHash :: CTxId
  , txhTimeIssued :: Maybe NominalDiffTime
  , txhAmount :: CCoin
  }


newtype TxBodyViewProps = TxBodyViewProps
  { txbInputs :: Array (Maybe (Tuple CAddress CCoin))
  , txbOutputs :: Array (Tuple CAddress CCoin)
  , txbAmount :: CCoin
  }
