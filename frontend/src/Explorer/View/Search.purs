module Explorer.View.Search
    ( searchInputView
    , searchItemViews
    ) where

import Prelude hiding (id)

import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Tuple (Tuple(..))
import DOM.Node.Types (ElementId(..))

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cAddress, cEpoch, cSlot, cTransaction, common, hero, hrSearch, hrTime) as I18nL
import Explorer.Lenses.State (gViewMobileMenuOpenend, gViewSearchInputFocused, gViewSearchQuery, gViewSearchTimeQuery, gViewSelectedSearch, globalViewState, lang, viewStates)
import Explorer.State (maxSlotInEpoch)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (Search(..), State)
import Explorer.View.Common (emptyView)

import Pux.DOM.HTML (Html) as P
import Pux.DOM.Events (onChange, onClick, onFocus, onBlur, onKey) as P

import Text.Smolder.HTML (div, input, label, li, text, ul)
import Text.Smolder.HTML.Attributes (className, for, id, maxLength, name, placeholder, type', value)
import Text.Smolder.Markup (text, (#!), (!))

inputEpochName :: String
inputEpochName = "inp_epoch"

inputSlotName :: String
inputSlotName = "inp_slot"

searchInputView :: ElementId -> State -> P.Html Action
searchInputView (ElementId viewId) state =
    let lang' = state ^. lang
        dbViewSearchInputFocused = state ^. (viewStates <<< globalViewState <<< gViewSearchInputFocused)
        mobileMenuOpened = state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)
        selectedSearch = state ^. (viewStates <<< globalViewState <<< gViewSelectedSearch)
        addrHiddenClazz = if selectedSearch == SearchTime  then " hide " else ""
        epochHiddenClazz = if selectedSearch /= SearchTime  then " hide " else ""
        focusedClazz = if dbViewSearchInputFocused then " focused " else ""
        searchTimeQuery = state ^. (viewStates <<< globalViewState <<< gViewSearchTimeQuery)
    in
    div ! className ("explorer-search__container" <> focusedClazz)
        ! id viewId $ do
        input ! className $ "explorer-search__input explorer-search__input--address-tx"
                          <> addrHiddenClazz <> focusedClazz
              ! type' "text"
              ! placeholder $ if dbViewSearchInputFocused
                              then ""
                              else translate (I18nL.hero <<< I18nL.hrSearch) lang'
              #! P.onFocus <<< const $
                  if mobileMenuOpened
                  then GlobalFocusSearchInput true
                  else NoOp
              #! P.onBlur <<< const $
                  if mobileMenuOpened
                  then GlobalFocusSearchInput false
                  else NoOp
              #! P.onChange $ GlobalUpdateSearchValue <<< _.value <<< _.target
              #! P.onKey "enter" $ const GlobalSearch
              #! P.value $ state ^. (viewStates <<< globalViewState <<< gViewSearchQuery)
        div ! className ("explorer-search__wrapper" <> epochHiddenClazz) $ do
            label ! className $ "explorer-search__label"
                  ! for inputEpochName
                  $ text (translate (I18nL.common <<< I18nL.cEpoch) lang')
            input ! className $ "explorer-search__input explorer-search__input--epoch"
                                  <> focusedClazz
                  ! type' "text"
                  ! name inputEpochName
                  #! P.onFocus <<< const $
                        if mobileMenuOpened
                        then GlobalFocusSearchInput true
                        else NoOp
                  #! P.onBlur <<< const $
                        if mobileMenuOpened
                        then GlobalFocusSearchInput false
                        else NoOp
                  #! P.onChange $ GlobalUpdateSearchEpochValue <<< _.value <<< _.target
                  #! P.onKey "enter" $ const GlobalSearchTime
                  #! P.value $ case searchTimeQuery of
                                    Tuple (Just epoch) _ -> show epoch
                                    _ -> ""
            label ! className $ "explorer-search__label"
                  ! for inputSlotName
                  $ text (translate (I18nL.common <<< I18nL.cSlot) lang')
            input ! className $ "explorer-search__input explorer-search__input--slot"
                                  <> focusedClazz
                  ! type' "text"
                  ! name inputSlotName
                  ! maxLength <<< show <<< length $ show maxSlotInEpoch
                  #! P.onFocus <<< const $ if mobileMenuOpened
                                                then GlobalFocusSearchInput true
                                                else NoOp
                  #! P.onBlur <<< const $ if mobileMenuOpened
                                                then GlobalFocusSearchInput false
                                                else NoOp
                  #! P.onChange $ GlobalUpdateSearchSlotValue <<< _.value <<< _.target
                  #! P.onKey "enter" $ const GlobalSearchTime
                  #! P.value $ case searchTimeQuery of
                                  Tuple _ (Just slot) -> show slot
                                  _ -> ""
        if dbViewSearchInputFocused
            then searchItemViews lang' selectedSearch
            else emptyView
        div ! className $ "explorer-search__btn bg-icon-search" <> focusedClazz
            #! P.onClick <<< const $ if selectedSearch == SearchTime
                                        then GlobalSearchTime
                                        else GlobalSearch

type SearchItem =
  { value :: Search
  , label :: String
  }

type SearchItems = Array SearchItem

mkSearchItems :: Language -> SearchItems
mkSearchItems lang =
    [ { value: SearchAddress
      , label: translate (I18nL.common <<< I18nL.cAddress) lang
      }
    , { value: SearchTx
      , label: translate (I18nL.common <<< I18nL.cTransaction) lang
      }
    , { value: SearchTime
      , label: translate (I18nL.hero <<< I18nL.hrTime) lang
      }
    ]

searchItemViews :: Language -> Search -> P.Html Action
searchItemViews lang selectedSearch =
    ul ! P.className $ "explorer-search-nav__container" $ do
        map (\item -> searchItemView item selectedSearch) $ mkSearchItems lang

searchItemView :: SearchItem -> Search -> P.Html Action
searchItemView item selectedSearch =
    let selected = item.value == selectedSearch
        selectedClass = if selected then " selected" else ""
    in
    li ! className $ "explorer-search-nav__item"  <> selectedClass
      #! P.onClick <<< const $ GlobalUpdateSelectedSearch item.value
      $ text item.label
