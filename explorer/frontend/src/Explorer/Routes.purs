module Explorer.Routes where

import Prelude

import Control.Alt ((<|>))
import Data.Generic (class Generic, gEq, gShow)
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Explorer.Util.Factory (mkCAddress, mkCHash, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Global (decodeURIComponent, encodeURIComponent)
import Pos.Core.Slotting.Lenses.Types (_EpochIndex, _UncheckedLocalSlotIndex, getEpochIndex, getSlotIndex)
import Pos.Core.Slotting.Types (EpochIndex, LocalSlotIndex)
import Pos.Explorer.Web.ClientTypes (CAddress, CHash, CTxId)
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CHash, _CTxId)
import Pux.Router (end, int, lit, router, str)

data Route
    = Dashboard
    | Tx CTxId
    | Address CAddress
    | EpochSlot EpochIndex LocalSlotIndex
    | Epoch EpochIndex
    | Calculator
    | Block CHash
    | GenesisBlock
    | Playground
    | NotFound

derive instance genericRoute :: Generic Route
instance eqRoute :: Eq Route where
    eq = gEq
instance sRoute :: Show Route where
    show = gShow

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Dashboard <$ end
    <|>
    Tx <<< mkCTxId <$> (lit transactionLit *> str) <* end
    <|>
    Address <<< mkCAddress <<< decodeURIComponent <$> (lit addressLit *> str) <* end
    <|>
    EpochSlot <$> mkEpochIndex <$> (lit epochLit *> int)
              <*> (mkLocalSlotIndex <$> (lit slotLit *> int) <* end)
    <|>
    Epoch <<< mkEpochIndex <$> (lit epochLit *> int) <* end
    <|>
    Calculator <$ (lit calculatorLit) <* end
    <|>
    Block <<< mkCHash <$> (lit slotLit *> str) <* end
    <|>
    GenesisBlock <$ (lit genesisBlockLit) <* end
    <|>
    -- TODO (jk) Disable Playground route in production mode
    -- It is just for debugging
    Playground <$ lit playgroundLit <* end

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl (Tx id) = transactionUrl id
toUrl (Address cAddress) = addressUrl cAddress
toUrl (EpochSlot epoch slot) = epochSlotUrl epoch slot
toUrl (Epoch epoch) = epochUrl epoch
toUrl Calculator = calculatorUrl
toUrl (Block hash) = blockUrl hash
toUrl GenesisBlock = genesisBlockUrl
toUrl Playground = playgroundUrl
toUrl NotFound = notFoundUrl

litUrl :: String -> String
litUrl lit = "/" <> lit <> "/"

dashboardLit :: String
dashboardLit = "dashboard"

dashboardUrl :: String
dashboardUrl = "/"

transactionLit :: String
transactionLit = "tx"

transactionUrl :: CTxId -> String
transactionUrl id = litUrl transactionLit <> id ^. (_CTxId <<< _CHash)

addressLit :: String
addressLit = "address"

addressUrl :: CAddress -> String
addressUrl address = litUrl addressLit <> (encodeURIComponent (address ^. _CAddress))

epochLit :: String
epochLit = "epoch"

slotLit :: String
slotLit = "slot"

epochUrl :: EpochIndex -> String
epochUrl epoch = litUrl epochLit <> paramToString epoch

epochSlotUrl :: EpochIndex -> LocalSlotIndex -> String
epochSlotUrl epoch slot = litUrl epochLit
                          <> paramToString epoch
                          <> litUrl slotLit
                          <> paramToString slot

calculatorLit :: String
calculatorLit = "calculator"

calculatorUrl :: String
calculatorUrl = "/" <> calculatorLit

blockUrl :: CHash -> String
blockUrl hash = litUrl slotLit <> hash ^. _CHash

genesisBlockLit :: String
genesisBlockLit = "genesis"

genesisBlockUrl :: String
genesisBlockUrl = "/" <> genesisBlockLit

playgroundLit :: String
playgroundLit = "playground"

playgroundUrl :: String
playgroundUrl = litUrl playgroundLit

notFoundLit :: String
notFoundLit = "404"

notFoundUrl :: String
notFoundUrl = "/" <> notFoundLit


class RouteParams a where
    paramToString :: a -> String

instance epochIndexRouteParams :: RouteParams EpochIndex where
    paramToString epoch =
        show (epoch ^. (_EpochIndex <<< getEpochIndex))

instance localSlotIndexRouteParams :: RouteParams LocalSlotIndex where
    paramToString slot =
        show (slot ^. (_UncheckedLocalSlotIndex <<< getSlotIndex))
