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
import Explorer.Types.State (AddressesFilter(..), CBlockEntries, CGenesisAddressInfos, CTxBriefs, CTxEntries, PageNumber(..), PageSize(..))
import Global (encodeURIComponent)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))
import Pos.Core.Slotting.Lenses.Types (_EpochIndex, _UnsafeLocalSlotIndex, getEpochIndex, getSlotIndex)
import Pos.Core.Slotting.Types (EpochIndex(..), LocalSlotIndex)
import Pos.Explorer.Web.ClientTypes (CAddress(..), CAddressSummary, CAddressesFilter(..), CBlockSummary, CGenesisSummary, CHash(..), CTxId, CTxSummary)
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
fetchBlockTxs (CHash hash) =
    get $ "blocks/txs/"
              <> hash
              <> "?limit=5000"
              -- ^ hardcoded limit as discussed in `CSL-2047`

-- txs
fetchLatestTxs :: forall eff. Aff (ajax::AJAX | eff) CTxEntries
fetchLatestTxs = get $ "txs/last"

fetchTxSummary :: forall eff. CTxId -> Aff (ajax::AJAX | eff) CTxSummary
fetchTxSummary id = get $ "txs/summary/" <> id ^. (_CTxId <<< _CHash)

-- addresses
fetchAddressSummary :: forall eff. CAddress -> Aff (ajax::AJAX | eff) CAddressSummary
fetchAddressSummary (CAddress address) = get $ "addresses/summary/" <> (encodeURIComponent address)

-- search by epoch / page
epochPageSearch :: forall eff. EpochIndex -> PageNumber -> Aff (ajax::AJAX | eff) (Tuple Int CBlockEntries)
epochPageSearch (EpochIndex epochIndex) (PageNumber pNumber) = get $ "epochs/"
    <> show (epochIndex ^. getEpochIndex) <> "?page=" <> show pNumber

-- search by epoch and slot
epochSlotSearch :: forall eff. EpochIndex -> LocalSlotIndex -> Aff (ajax::AJAX | eff) CBlockEntries
epochSlotSearch epoch slot = get $ "epochs/" <> show epochIndex <> slotQuery
    where
        slotQuery = "/" <> show (slot ^. (_UnsafeLocalSlotIndex <<< getSlotIndex))
        epochIndex = epoch ^. (_EpochIndex <<< getEpochIndex)

-- genesis block
fetchGenesisSummary :: forall eff. Aff (ajax::AJAX | eff) CGenesisSummary
fetchGenesisSummary = get "genesis/summary/"

fetchGenesisAddressInfo :: forall eff. PageNumber -> PageSize -> AddressesFilter -> Aff (ajax::AJAX | eff) CGenesisAddressInfos
fetchGenesisAddressInfo (PageNumber pNumber) (PageSize pSize) (AddressesFilter cAddrFilter) =
    get $ "genesis/address/?page="
              <> show pNumber
              <> "&pageSize="
              <> show pSize
              <> "&filter="
              <> filterGenesisAddressesValue cAddrFilter

fetchGenesisAddressInfoTotalPages :: forall eff. PageSize -> AddressesFilter -> Aff (ajax::AJAX | eff) Int
fetchGenesisAddressInfoTotalPages (PageSize pSize) (AddressesFilter cAddrFilter) =
    get $ "genesis/address/pages/total?pageSize="
              <> show pSize
              <> "&filter="
              <> filterGenesisAddressesValue cAddrFilter

filterGenesisAddressesValue :: CAddressesFilter -> String
filterGenesisAddressesValue AllAddresses = "all"
filterGenesisAddressesValue RedeemedAddresses = "redeemed"
filterGenesisAddressesValue NonRedeemedAddresses = "notredeemed"
