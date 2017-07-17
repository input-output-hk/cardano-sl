
-- | Detail page of a `genesis block` https://cardanodocs.com/technical/blocks/#genesis-block

module Explorer.View.GenesisBlock
    ( genesisBlockView
    )
    where

import Prelude

import Data.Array (length, null, slice)
import Data.Foldable (for_)
import Data.Lens ((^.))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cGenesis, cAddresses, cOf, common, cLoading, cSummary, gblAddressesEmpty, gblAddressHash, gblAddressesNotFound, gblAddressRedeemAmount, gblAddressIsRedeemed, gblNotFound, genesisBlock) as I18nL
import Explorer.Lenses.State (_PageNumber, currentCGenesisAddressInfos, currentCGenesisSummary, gblAddressPagination, gblAddressPaginationEditable, genesisBlockViewState, lang, viewStates)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CGenesisAddressInfos, PageNumber(..), State)
import Explorer.View.Common (getMaxPaginationNumber, paginationView)
import Network.RemoteData (RemoteData(..))
import Pos.Explorer.Web.ClientTypes (CGenesisAddressInfo, CGenesisSummary)
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (div, h3) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as S

genesisBlockView :: State -> P.HTML Action
genesisBlockView state =
    let lang' = state ^. lang
    in
    S.div ! S.className "explorer-genesis"$ do
        S.div ! S.className "explorer-genesis__wrapper"
              $ S.div ! S.className "explorer-genesis__container" $ do
                    S.h3  ! S.className "headline"
                        $ S.text (translate (I18nL.common <<< I18nL.cGenesis) lang')
                    case state ^. currentCGenesisSummary of
                        NotAsked  -> emptyView ""
                        Loading   -> emptyView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                        Failure _ -> emptyView $ translate (I18nL.genesisBlock <<< I18nL.gblNotFound) lang'
                        Success summary -> summaryView summary lang'

        S.div ! S.className "explorer-genesis__wrapper" $ do
            S.div ! S.className "explorer-genesis__container" $ do
                S.h3  ! S.className "headline"
                      $ S.text (translate (I18nL.common <<< I18nL.cAddresses) lang')
                case state ^. currentCGenesisAddressInfos of
                    NotAsked  -> emptyView ""
                    Loading   -> emptyView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                    Failure _ -> emptyView $ translate (I18nL.genesisBlock <<< I18nL.gblAddressesNotFound) lang'
                    Success addresses -> addressesView addresses state


emptyView :: String -> P.HTML Action
emptyView message =
    S.div ! S.className "summary-empty__container"
          $ S.text message

summaryView :: CGenesisSummary -> Language -> P.HTML Action
summaryView summary lang =
    S.div ! S.className "summary-wrapper" $ do
        S.div ! S.className "summary-container" $ do
            S.h3  ! S.className "subheadline"
                  $ S.text (translate (I18nL.common <<< I18nL.cSummary) lang)


type AddressesHeaderProps =
    { id :: String
    , label :: String
    , clazz :: String
    }

mkAddressesHeaderProps :: Language -> Array AddressesHeaderProps
mkAddressesHeaderProps lang =
    [ { id: "0"
      , label: translate (I18nL.genesisBlock <<< I18nL.gblAddressHash) lang
      , clazz: "hash"
      }
    , { id: "1"
      , label: translate (I18nL.genesisBlock <<< I18nL.gblAddressRedeemAmount) lang
      , clazz: "amount"
      }
    , { id: "2"
      , label: translate (I18nL.genesisBlock <<< I18nL.gblAddressIsRedeemed) lang
      , clazz: "redeemed"
      }
    ]

addressesHeaderView :: Language -> P.HTML Action
addressesHeaderView lang =
    S.div ! S.className "addresses-header"
          $ for_ (mkAddressesHeaderProps lang) addressesHeaderItemView

addressesHeaderItemView :: AddressesHeaderProps -> P.HTML Action
addressesHeaderItemView props =
    S.div ! S.className props.clazz
          ! P.key props.id
          $ S.text props.label

maxAddrRows :: Int
maxAddrRows = 5

addressesView :: CGenesisAddressInfos -> State -> P.HTML Action
addressesView addresses state =
    if null addresses then
        emptyView $ translate (I18nL.genesisBlock <<< I18nL.gblAddressesEmpty) (state ^. lang)
    else
        let addrPagination = state ^. (viewStates <<< genesisBlockViewState <<< gblAddressPagination <<< _PageNumber)
            lang' = state ^. lang
            minAddrIndex = (addrPagination - minPagination) * maxAddrRows
            currentAddrs = slice minAddrIndex (minAddrIndex + maxAddrRows) addresses
        in
        do
            addressesHeaderView lang'
            for_ currentAddrs (\address -> addressView address lang')
            paginationView  { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                              , currentPage: PageNumber addrPagination
                              , minPage: PageNumber minPagination
                              , maxPage: PageNumber $ getMaxPaginationNumber (length addresses) maxAddrRows
                              , changePageAction: GenesisBlockPaginateAddresses
                              , editable: state ^. (viewStates <<< genesisBlockViewState <<< gblAddressPaginationEditable)
                              , editableAction: GenesisBlockEditAddressesPageNumber
                              , invalidPageAction: GenesisBlockInvalidAddressesPageNumber
                              , disabled: false
                              }

addressView :: CGenesisAddressInfo -> Language -> P.HTML Action
addressView address state =
    S.div ! S.className "explorer-genesis__address-container"
          $ S.text "Address"
