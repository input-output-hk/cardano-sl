module Explorer.View.Search
    ( searchInputView
    , searchItemViews
    ) where

import Prelude


import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (length)
import Data.Tuple (Tuple(..))
import DOM.Node.Types (ElementId(..))

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cAddress, cEpoch, cSlot, cTransaction, common, hero, hrSearch, hrTime) as I18nL
import Explorer.Lenses.State (gViewMobileMenuOpenend, gViewSearchInputFocused, gViewSearchQuery, gViewSearchTimeQuery, gViewSelectedSearch, globalViewState, lang, viewStates)
import Explorer.State (maxSlotInEpoch)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (Search(..), State)
import Explorer.Util.DOM (enterKeyPressed)
import Explorer.View.Common (emptyView)

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onChange, onClick, onFocus, onBlur, onKeyDown, targetValue) as P

import Text.Smolder.HTML (div, input, label, li, ul) as S
import Text.Smolder.HTML.Attributes (className, for, id, maxlength, name, placeholder, type', value) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((#!), (!), (!?))

inputEpochName :: String
inputEpochName = "inp_epoch"

inputSlotName :: String
inputSlotName = "inp_slot"

searchInputView :: ElementId -> State -> P.HTML Action
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
    S.div ! S.className ("explorer-search__container" <> focusedClazz)
          ! S.id viewId $ do
          (S.input !? dbViewSearchInputFocused) (S.placeholder $ translate (I18nL.hero <<< I18nL.hrSearch) lang')
                  ! S.className ("explorer-search__input explorer-search__input--address-tx"
                                    <> addrHiddenClazz <> focusedClazz)
                  ! S.type' "text"
                  ! S.value (state ^. (viewStates <<< globalViewState <<< gViewSearchQuery))
                  #! P.onFocus (const $ if mobileMenuOpened
                                            then GlobalFocusSearchInput true
                                            else NoOp)
                  #! P.onBlur (const $ if mobileMenuOpened
                                          then GlobalFocusSearchInput false
                                          else NoOp)
                  #! P.onChange (GlobalUpdateSearchValue <<< P.targetValue)
                  #! P.onKeyDown (\event -> if enterKeyPressed event then GlobalSearch event else NoOp)
          S.div ! S.className ("explorer-search__wrapper" <> epochHiddenClazz) $ do
              S.label ! S.className "explorer-search__label"
                      ! S.for inputEpochName
                      $ S.text (translate (I18nL.common <<< I18nL.cEpoch) lang')
              S.input ! S.className ("explorer-search__input explorer-search__input--epoch"
                                        <> focusedClazz)
                    ! S.type' "text"
                    ! S.name inputEpochName
                    #! P.onFocus (const $ if mobileMenuOpened
                                              then GlobalFocusSearchInput true
                                              else NoOp)
                    #! P.onBlur (const $ if mobileMenuOpened
                                              then GlobalFocusSearchInput false
                                              else NoOp)
                    #! P.onChange (GlobalUpdateSearchEpochValue <<< P.targetValue)
                    #! P.onKeyDown (\event -> if enterKeyPressed event then GlobalSearchTime event else NoOp)
                    ! S.value (case searchTimeQuery of
                                      Tuple (Just epoch) _ -> show epoch
                                      _ -> "")
              S.label ! S.className "explorer-search__label"
                      ! S.for inputSlotName
                      $ S.text (translate (I18nL.common <<< I18nL.cSlot) lang')
              S.input ! S.className ("explorer-search__input explorer-search__input--slot"
                                        <> focusedClazz)
                      ! S.type' "text"
                      ! S.name inputSlotName
                      ! S.maxlength (show <<< length $ show maxSlotInEpoch)
                      #! P.onFocus (const $ if mobileMenuOpened
                                                then GlobalFocusSearchInput true
                                                else NoOp)
                      #! P.onBlur (const $ if mobileMenuOpened
                                                then GlobalFocusSearchInput false
                                                else NoOp)
                      #! P.onChange (GlobalUpdateSearchSlotValue <<< P.targetValue)
                      #! P.onKeyDown (\event -> if enterKeyPressed event
                                                    then GlobalSearchTime event
                                                    else NoOp)
                      ! S.value (case searchTimeQuery of
                                      Tuple _ (Just slot) -> show slot
                                      _ -> "")
          if dbViewSearchInputFocused
              then searchItemViews lang' selectedSearch
              else emptyView
          S.div ! S.className ("explorer-search__btn bg-icon-search" <> focusedClazz)
                #! P.onClick (if selectedSearch == SearchTime
                                  then GlobalSearchTime
                                  else GlobalSearch)
                $ mempty

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

searchItemViews :: Language -> Search -> P.HTML Action
searchItemViews lang selectedSearch =
    S.ul  ! S.className "explorer-search-nav__container"
          $ for_ (mkSearchItems lang) (\item -> searchItemView item selectedSearch)

searchItemView :: SearchItem -> Search -> P.HTML Action
searchItemView item selectedSearch =
    let selected = item.value == selectedSearch
        selectedClass = if selected then " selected" else ""
    in
    S.li  ! S.className ("explorer-search-nav__item"  <> selectedClass)
          #! P.onClick (const $ GlobalUpdateSelectedSearch item.value)
          $ S.text item.label
