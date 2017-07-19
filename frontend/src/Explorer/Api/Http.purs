module Explorer.Api.Http where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), either)
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Explorer.Api.Helper (decodeResult)
import Explorer.Api.Types (Endpoint, EndpointError(..))
import Explorer.Types.State (CBlockEntries, CTxBriefs, CTxEntries, PageNumber(..), PageSize(..), CGenesisAddressInfos)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))
import Pos.Core.Lenses.Types (_EpochIndex, _LocalSlotIndex, getEpochIndex, getSlotIndex)
import Pos.Core.Types (EpochIndex, LocalSlotIndex)
import Pos.Explorer.Web.ClientTypes (CAddress(..), CAddressSummary, CBlockSummary, CGenesisSummary, CHash(..), CTxId, CTxSummary)
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash, _CTxId)

endpointPrefix :: String
endpointPrefix = "/api/"

-- result helper

decodeResponse :: forall a eff. Generic a => {response :: Json | eff} -> Either Error a
decodeResponse = decodeResult <<< _.response

request :: forall a r eff. Generic a => Requestable r => AffjaxRequest r ->
    Endpoint -> Aff (ajax :: AJAX | eff) a
request req endpoint = do
    result <- affjax $ req { url = endpointPrefix <> endpoint }
    when (isHttpError result.status) $
        throwError <<< error <<< show $ HTTPStatusError result
    either throwError pure $ decodeResponse result
    where
      isHttpError (StatusCode c) = c >= 400

get :: forall eff a. Generic a => Endpoint -> Aff (ajax :: AJAX | eff) a
get e = request defaultRequest e

post :: forall eff a. Generic a => Endpoint -> Aff (ajax :: AJAX | eff) a
post = request $ defaultRequest { method = Left POST }

-- api

-- blocks

fetchBlocksTotalPages :: forall eff. Aff (ajax::AJAX | eff) Int
fetchBlocksTotalPages = get "blocks/pages/total"

fetchPageBlocks :: forall eff. PageNumber -> PageSize -> Aff (ajax::AJAX | eff) (Tuple Int CBlockEntries)
fetchPageBlocks (PageNumber pNumber) (PageSize pSize) =
    get $ "blocks/pages/?page=" <> show pNumber <> "&pageSize=" <> show pSize

fetchBlockSummary :: forall eff. CHash -> Aff (ajax::AJAX | eff) CBlockSummary
fetchBlockSummary (CHash hash) = get $ "blocks/summary/" <> hash

fetchBlockTxs :: forall eff. CHash -> Aff (ajax::AJAX | eff) CTxBriefs
fetchBlockTxs (CHash hash) = get $ "blocks/txs/" <> hash

-- txs
fetchLatestTxs :: forall eff. Aff (ajax::AJAX | eff) CTxEntries
fetchLatestTxs = get $ "txs/last"

fetchTxSummary :: forall eff. CTxId -> Aff (ajax::AJAX | eff) CTxSummary
fetchTxSummary id = get $ "txs/summary/" <> id ^. (_CTxId <<< _CHash)

-- addresses
fetchAddressSummary :: forall eff. CAddress -> Aff (ajax::AJAX | eff) CAddressSummary
fetchAddressSummary (CAddress address) = get $ "addresses/summary/" <> (encodeURIComponent address)

-- search by epoch / slot
searchEpoch :: forall eff. EpochIndex -> Maybe LocalSlotIndex -> Aff (ajax::AJAX | eff) CBlockEntries
searchEpoch epoch mSlot = get $ "search/epoch/" <> show epochIndex <> slotQuery mSlot
  where
      slotQuery Nothing = ""
      slotQuery (Just slot) = "?slot=" <> show (slot ^. (_LocalSlotIndex <<< getSlotIndex))

      epochIndex = epoch ^. (_EpochIndex <<< getEpochIndex)

-- genesis block
fetchGenesisSummary :: forall eff. Aff (ajax::AJAX | eff) CGenesisSummary
fetchGenesisSummary = get "genesis/summary/"

fetchGenesisAddressInfo :: forall eff. PageNumber -> PageSize -> Aff (ajax::AJAX | eff) CGenesisAddressInfos
fetchGenesisAddressInfo (PageNumber pNumber) (PageSize pSize) =
    get $ "genesis/address/?page=" <> show pNumber <> "&pageSize=" <> show pSize

fetchGenesisAddressInfoTotalPages :: forall eff. PageSize -> Aff (ajax::AJAX | eff) Int
fetchGenesisAddressInfoTotalPages (PageSize pSize)= get $ "genesis/address/pages/total?pageSize=" <> show pSize
