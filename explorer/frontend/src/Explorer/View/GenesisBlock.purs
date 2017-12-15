
-- | Detail page of a `genesis block` https://cardanodocs.com/technical/blocks/#genesis-block

module Explorer.View.GenesisBlock
    ( genesisBlockView
    , maxAddressInfoRows
    )
    where

import Prelude

import Data.Array (null)
import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), isJust)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cGenesis, cAddress, cAddresses, cOf, common, cLoading, cNo, cSummary, cYes, gblAddressesError, gblAddressesNotFound, gblAddressRedeemAmount, gblAddressIsRedeemed, gblFilterAll, gblFilterRedeemed, gblFilterNonRedeemed, gblNonRedeemedAmountTotal, gblNotFound, gblNumberAddressesToRedeem, gblNumberRedeemedAddresses, gblNumberNotRedeemedAddresses, gblRedeemedAmountTotal, genesisBlock) as I18nL
import Explorer.Lenses.State (currentCGenesisAddressInfos, currentCGenesisSummary, gblAddressFilter, gblLoadingAddressInfosPagination, gblAddressInfosPagination, gblAddressInfosPaginationEditable, gblMaxAddressInfosPagination, genesisBlockViewState, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (AddressesFilter(..), CCurrency(..), CGenesisAddressInfos, PageNumber(..), State)
import Explorer.Util.String (formatADA)
import Explorer.View.Common (currencyCSSClass, paginationView)
import Network.RemoteData (RemoteData(..), withDefault)
import Pos.Explorer.Web.ClientTypes (CAddressesFilter(..), CGenesisAddressInfo(..), CGenesisSummary(..), _CCoin)
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, cgaiCardanoAddress, cgaiGenesisAmount, cgaiIsRedeemed, cgsNonRedeemedAmountTotal, cgsNumNotRedeemed, cgsNumRedeemed, cgsNumTotal, cgsRedeemedAmountTotal, getCoin)
import Pux.DOM.Events (DOMEvent, onClick) as P
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (a, div, h3, li, p, span, ul) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup ((!), (#!))
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
                    Failure _ -> emptyView $ translate (I18nL.genesisBlock <<< I18nL.gblAddressesError) lang'
                    Success infos -> addressInfosView infos state


emptyView :: String -> P.HTML Action
emptyView message =
    S.div ! S.className "summary-empty__container"
          $ S.text message

type SummaryRowProps =
    { key :: Int
    , label :: String
    , amount :: String
    , mCurrency :: Maybe CCurrency
    }

mkPropsForSummaryRows :: Language -> CGenesisSummary -> Array SummaryRowProps
mkPropsForSummaryRows lang (CGenesisSummary summary) =
    [ { key: 0
      , label: translate (I18nL.genesisBlock <<< I18nL.gblNumberAddressesToRedeem) lang
      , amount: show $ summary ^. cgsNumTotal
      , mCurrency: Nothing
      }
    , { key: 1
      , label: translate (I18nL.genesisBlock <<< I18nL.gblNumberRedeemedAddresses) lang
      , amount: show $ summary ^. cgsNumRedeemed
      , mCurrency: Nothing
      }
    , { key: 2
      , label: translate (I18nL.genesisBlock <<< I18nL.gblNumberNotRedeemedAddresses) lang
      , amount: show $ summary ^. cgsNumNotRedeemed
      , mCurrency: Nothing
      }
    , { key: 3
      , label: translate (I18nL.genesisBlock <<< I18nL.gblRedeemedAmountTotal) lang
      , amount: formatADA (summary ^. cgsRedeemedAmountTotal) lang
      , mCurrency: Just ADA
      }
    , { key: 4
      , label: translate (I18nL.genesisBlock <<< I18nL.gblNonRedeemedAmountTotal) lang
      , amount: formatADA (summary ^. cgsNonRedeemedAmountTotal) lang
      , mCurrency: Just ADA
      }
    ]

summaryRow :: SummaryRowProps -> P.HTML Action
summaryRow props =
    S.div
        ! S.className "row row__summary"
        ! P.key (show props.key)
        $ do
            S.div ! S.className "column column__label"
                  $ S.text props.label
            S.div ! S.className "column column__amount"
                  $ if isJust props.mCurrency
                        then S.span ! S.className (currencyCSSClass props.mCurrency)
                                    $ S.text props.amount
                        else S.text props.amount

summaryView :: CGenesisSummary -> Language -> P.HTML Action
summaryView summary lang =
    S.div ! S.className "summary-wrapper" $ do
        S.div ! S.className "summary-container" $ do
            S.h3  ! S.className "subheadline"
                  $ S.text (translate (I18nL.common <<< I18nL.cSummary) lang)
            S.div $ for_ (mkPropsForSummaryRows lang summary) summaryRow

type AddressInfosHeaderProps =
    { key :: Int
    , label :: String
    , clazz :: String
    }

mkPropsForAddressHeaderView :: Language -> Array AddressInfosHeaderProps
mkPropsForAddressHeaderView lang =
    [ { key: 0
      , label: translate (I18nL.common <<< I18nL.cAddress) lang
      , clazz: "hash"
      }
    , { key: 1
      , label: translate (I18nL.genesisBlock <<< I18nL.gblAddressRedeemAmount) lang
      , clazz: "amount"
      }
    , { key: 2
      , label: translate (I18nL.genesisBlock <<< I18nL.gblAddressIsRedeemed) lang
      , clazz: "redeemed"
      }
    ]

addressesHeaderView :: Language -> P.HTML Action
addressesHeaderView lang =
    S.div ! S.className "address-infos__header"
          $ for_ (mkPropsForAddressHeaderView lang) addressesHeaderItemView

addressesHeaderItemView :: AddressInfosHeaderProps -> P.HTML Action
addressesHeaderItemView props =
    S.div ! S.className props.clazz
          ! P.key (show props.key)
          $ S.text props.label

addressInfosBodyView :: Language -> CGenesisAddressInfo -> P.HTML Action
addressInfosBodyView lang (CGenesisAddressInfo info) =
    let addrLink = toUrl $ Address (info ^. cgaiCardanoAddress)
        addrString = info ^. (cgaiCardanoAddress <<< _CAddress)
    in
    S.div ! S.className "address-infos__body--row"
          ! P.key addrString
          $ do
          -- hash
          S.a ! S.className "address-infos__body--column hash"
              ! S.href addrLink
              #! P.onClick (Navigate addrLink)
              $ S.text addrString
          -- amount
          S.div ! S.className "address-infos__body--column amount"
                $ S.span ! S.className (currencyCSSClass $ Just ADA)
                          $ S.text (formatADA (info ^. cgaiGenesisAmount) lang)
          -- is redeemed
          S.div ! S.className "address-infos__body--column redeemed"
                $ S.text (if (info ^. cgaiIsRedeemed)
                              then translate (I18nL.common <<< I18nL.cYes) lang
                              else translate (I18nL.common <<< I18nL.cNo) lang)


type AddressFilterNavItemProps =
    { key :: Int
    , label :: String
    , filter :: AddressesFilter
    }

mkPropsForAddressFilterNavItem :: Language -> Array AddressFilterNavItemProps
mkPropsForAddressFilterNavItem lang =
    [ { key: 0
      , label: translate (I18nL.genesisBlock <<< I18nL.gblFilterAll) lang
      , filter: AddressesFilter AllAddresses
      }
    , { key: 1
      , label: translate (I18nL.genesisBlock <<< I18nL.gblFilterRedeemed) lang
      , filter: AddressesFilter RedeemedAddresses
      }
    , { key: 2
      , label: translate (I18nL.genesisBlock <<< I18nL.gblFilterNonRedeemed) lang
      , filter: AddressesFilter NonRedeemedAddresses
      }
    ]

addressFilterNav :: AddressesFilter -> Language -> P.HTML Action
addressFilterNav selectedFilter lang =
    S.ul ! S.className "address-filter__nav jk"
          $ for_ (mkPropsForAddressFilterNavItem lang) (addressFilterNavItem selectedFilter)

addressFilterNavItem :: AddressesFilter -> AddressFilterNavItemProps -> P.HTML Action
addressFilterNavItem selectedFilter props =
    let selected = props.filter == selectedFilter
        selectedClazz = if selected then "disabled" else ""
        clickHandler :: P.DOMEvent -> Action
        clickHandler _ =
            if not selected
            then GenesisBlockFilterAddresses props.filter
            else NoOp
    in
    S.li  ! S.className ("address-filter__nav--item " <>  selectedClazz)
          #! P.onClick clickHandler
          $ S.text props.label

maxAddressInfoRows :: Int
maxAddressInfoRows = 5

addressInfosView :: CGenesisAddressInfos -> State -> P.HTML Action
addressInfosView infos state =
    if null infos then
        emptyView $ translate (I18nL.genesisBlock <<< I18nL.gblAddressesNotFound) (state ^. lang)
    else
        let lang' = state ^. lang
            isLoading = state ^. (viewStates <<< genesisBlockViewState <<< gblLoadingAddressInfosPagination)
            addrFilter = state ^. (viewStates <<< genesisBlockViewState <<< gblAddressFilter)
        in
        do
            S.div
                ! S.className "address-infos__wrapper"
                $ do
                    addressFilterNav addrFilter lang'
                    addressesHeaderView lang'
                    S.div ! S.className "address-infos__body--wrapper"
                          $ do
                          for_ infos (addressInfosBodyView lang')
                          S.div ! S.className ("address-infos__body--cover" <>  if isLoading then " show" else "")
                                $ S.p ! S.className "address-infos__body--cover-label"
                                      $ S.text (translate (I18nL.common <<< I18nL.cLoading) lang')
            S.div
                ! S.className "address-infos__footer"
                $  paginationView  { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                                    , currentPage: state ^. (viewStates <<< genesisBlockViewState <<< gblAddressInfosPagination)
                                    , minPage: PageNumber minPagination
                                    , maxPage: withDefault
                                                  (PageNumber minPagination)
                                                  (state ^. (viewStates <<< genesisBlockViewState <<< gblMaxAddressInfosPagination))
                                    , changePageAction: GenesisBlockPaginateAddresses
                                    , editable: state ^. (viewStates <<< genesisBlockViewState <<< gblAddressInfosPaginationEditable)
                                    , editableAction: GenesisBlockEditAddressesPageNumber
                                    , invalidPageAction: GenesisBlockInvalidAddressesPageNumber
                                    , disabled: isLoading
                                    }
