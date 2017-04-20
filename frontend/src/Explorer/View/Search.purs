module Explorer.View.Search
    (searchInputView
    ) where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Tuple (Tuple(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cAddress, cEpoch, cSlot, cTransaction, common, hero, hrSearch, hrTime) as I18nL
import Explorer.Lenses.State (gViewSearchInputFocused, globalViewState, lang, gViewSearchQuery, gViewSearchTimeQuery, gViewSelectedSearch, viewStates)
import Explorer.State (maxSlotInEpoch)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (Search(..), State)
import Pux.Html (Html, div, text, ul, li, label, input) as P
import Pux.Html.Attributes (checked, className, htmlFor, id_, maxLength, name, type_, placeholder, value) as P
import Pux.Html.Events (onChange, onClick, onFocus, onBlur, onKey) as P

inputEpochName :: String
inputEpochName = "inp_epoch"

inputSlotName :: String
inputSlotName = "inp_slot"

searchInputView :: State -> P.Html Action
searchInputView state =
    let lang' = state ^. lang
        dbViewSearchInputFocused = state ^. (viewStates <<< globalViewState <<< gViewSearchInputFocused)
        searchIconClazz = if dbViewSearchInputFocused then " bg-icon-search-hover" else " bg-icon-search"
        selectedSearch = state ^. (viewStates <<< globalViewState <<< gViewSelectedSearch)
        addrHiddenClazz = if selectedSearch == SearchTime  then " hide " else ""
        epochHiddenClazz = if selectedSearch /= SearchTime  then " hide " else ""
        focusedClazz = if dbViewSearchInputFocused then " focused " else ""
        searchTimeQuery = state ^. (viewStates <<< globalViewState <<< gViewSearchTimeQuery)
    in
    P.div
        [ P.className $ "hero-search__container" <> focusedClazz ]
        [ P.input
            [ P.className $ "hero-search__input hero-search__input--address-tx"
                          <> addrHiddenClazz
                          <> focusedClazz
            , P.type_ "text"
            , P.placeholder $ if dbViewSearchInputFocused
                              then ""
                              else translate (I18nL.hero <<< I18nL.hrSearch) lang'
            , P.onFocus <<< const $ GlobalFocusSearchInput true
            , P.onBlur <<< const $ GlobalFocusSearchInput false
            , P.onChange $ GlobalUpdateSearchValue <<< _.value <<< _.target
            , P.onKey "enter" $ const GlobalSearch
            , P.value $ state ^. (viewStates <<< globalViewState <<< gViewSearchQuery)
            ]
            []
        , P.div
            [ P.className $ "hero-search__wrapper" <> epochHiddenClazz ]
            [ P.label
                [ P.className $ "hero-search__label"
                , P.htmlFor inputEpochName
                ]
                [ P.text $ translate (I18nL.common <<< I18nL.cEpoch) lang' ]
            , P.input
                [ P.className $ "hero-search__input hero-search__input--epoch"
                              <> focusedClazz
                , P.type_ "text"
                , P.name inputEpochName
                , P.onFocus <<< const $ GlobalFocusSearchInput true
                , P.onBlur <<< const $ GlobalFocusSearchInput false
                , P.onChange $ GlobalUpdateSearchEpochValue <<< _.value <<< _.target
                , P.onKey "enter" $ const GlobalSearchTime
                , P.value $ case searchTimeQuery of
                            Tuple (Just epoch) _ -> show epoch
                            _ -> ""
                ]
                []
            , P.label
                [ P.className $ "hero-search__label"
                , P.htmlFor inputSlotName
                ]
                [ P.text $ translate (I18nL.common <<< I18nL.cSlot) lang' ]
            , P.input
                [ P.className $ "hero-search__input hero-search__input--slot"
                              <> focusedClazz
                , P.type_ "text"
                , P.name inputSlotName
                , P.maxLength <<< show <<< length $ show maxSlotInEpoch
                , P.onFocus <<< const $ GlobalFocusSearchInput true
                , P.onBlur <<< const $ GlobalFocusSearchInput false
                , P.onChange $ GlobalUpdateSearchSlotValue <<< _.value <<< _.target
                , P.onKey "enter" $ const GlobalSearchTime
                , P.value $ case searchTimeQuery of
                            Tuple _ (Just slot) -> show slot
                            _ -> ""
                ]
                []
            ]
        , P.ul
            [ P.className "hero-search-nav__container"]
            <<< map (\item -> searchItemView item selectedSearch)
                $ mkSearchItems lang'
        , P.div
            [ P.className $ "hero-search__btn" <> searchIconClazz <> focusedClazz
            , P.onClick <<< const $ if selectedSearch == SearchTime
                                    then GlobalSearchTime
                                    else GlobalSearch
            ]
            []
        ]


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

searchItemView :: SearchItem -> Search -> P.Html Action
searchItemView item selectedSearch =
    let selected = item.value == selectedSearch
        selectedClass = if selected then " selected" else ""
    in
    P.li
        [ P.className "hero-search-nav__item" ]
        [ P.input
            [ P.type_ "radio"
            , P.id_ $ show item.value
            , P.name $ show item.value
            , P.onChange <<< const $ GlobalUpdateSelectedSearch item.value
            , P.checked $ item.value == selectedSearch
            , P.value $ show item.value
            ]
            []
        , P.label
            [ P.className $ "hero-search-nav__item--label" <> selectedClass
            , P.htmlFor $ show item.value ]
            [ P.text item.label ]
        ]
