module Explorer.Util.DOM
    ( currencyCSSClass
    ) where


import Data.Maybe (Maybe(..))
import Explorer.State (CCurrency(..))

currencyCSSClass :: Maybe CCurrency -> String
currencyCSSClass mCurrency =
  case mCurrency of
      Just ADA -> " ada bg-ada-dark"
      _ -> ""
