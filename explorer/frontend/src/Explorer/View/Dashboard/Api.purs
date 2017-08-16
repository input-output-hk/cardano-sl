module Explorer.View.Dashboard.Api (apiView) where

import Prelude

import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.Monoid (mempty)
import Data.Map (Map, fromFoldable, lookup) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (dbAboutBlockchain, dbAboutBlockchainDescription, dbCurl
    , dbGetAddress, dbGetApiKey, dbMoreExamples, dbJQuery, dbNode, dbResponse
    , cApi, common, dashboard) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (DashboardAPICode(..), State)
import Explorer.View.Dashboard.Lenses (dashboardSelectedApiCode)
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))

import Text.Smolder.HTML (div, h3, p, code) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((#!), (!))

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onClick) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P

type ApiTabLabel = String

type ApiCode =
    { getAddress :: String
    , response :: String
    }

emptyApiCode :: ApiCode
emptyApiCode = { getAddress: "", response: ""}

apiCodes :: M.Map DashboardAPICode ApiCode
apiCodes =
    M.fromFoldable (
      [ Tuple Curl { getAddress: "Curl getAddress", response: "{\n\t\"hash\": }"}
      , Tuple Node { getAddress: "Node getAddress", response: "Node response ..."}
      , Tuple JQuery { getAddress: "jQuery getAddress", response: "jQuery response ..."}
      ]
    )

apiView :: State -> P.HTML Action
apiView state =
    S.div ! S.className "explorer-dashboard__wrapper" $ do
          S.div ! S.className "explorer-dashboard__container" $ do
              headerView state $ headerOptions lang'
              S.div ! S.className "api-content" $ do
                  S.div ! S.className "api-content__container api-code" $ do
                      S.div ! S.className "api-code__tabs" $ do
                          for_ (apiTabs lang') (apiCodeTabView state)
                          apiCodeSnippetView (translate (I18nL.dashboard <<< I18nL.dbGetAddress) lang') addressSnippet
                          apiCodeSnippetView (translate (I18nL.dashboard <<< I18nL.dbResponse) lang') responseSnippet
                  S.div ! S.className "api-content__container api-about" $ do
                      S.h3  ! S.className "api-about__headline"
                            $ S.text (translate (I18nL.dashboard <<< I18nL.dbAboutBlockchain) lang')
                      S.p ! S.className "api-about__description"
                          ! P.dangerouslySetInnerHTML (translate (I18nL.dashboard <<< I18nL.dbAboutBlockchainDescription) lang')
                          $ mempty
                  S.div ! S.className "api-about__button"
                        $ S.text (translate (I18nL.dashboard <<< I18nL.dbGetApiKey) lang')
      where
        apiCode :: ApiCode
        apiCode = fromMaybe emptyApiCode $ M.lookup (state ^. dashboardSelectedApiCode) apiCodes
        lang' = state ^. lang
        addressSnippet = _.getAddress $ apiCode
        responseSnippet = _.response $ apiCode
        headerOptions lang = HeaderOptions
            { headline: translate (I18nL.common <<< I18nL.cApi) lang
            , link: Just $ HeaderLink { label: translate (I18nL.dashboard <<< I18nL.dbMoreExamples) lang', action: NoOp }
            }

apiTabs :: Language -> Array (Tuple DashboardAPICode ApiTabLabel)
apiTabs lang =
    [ Tuple Curl $ translate (I18nL.dashboard <<< I18nL.dbCurl) lang
    , Tuple Node $ translate (I18nL.dashboard <<< I18nL.dbNode) lang
    , Tuple JQuery $ translate (I18nL.dashboard <<< I18nL.dbJQuery) lang
    ]

apiCodeTabView :: State -> Tuple DashboardAPICode ApiTabLabel -> P.HTML Action
apiCodeTabView state (Tuple code label) =
    S.div ! S.className ("api-code__tab " <> selectedClazz)
          #! P.onClick (const $ DashboardShowAPICode code)
          $ S.text label
    where
      selectedClazz = if state ^. dashboardSelectedApiCode == code then "selected" else ""

apiCodeSnippetView :: String -> String -> P.HTML Action
apiCodeSnippetView headline snippet =
    S.div ! S.className "api-snippet" $ do
        S.h3  ! S.className "api-snippet__headline"
              $ S.text headline
        S.code  ! S.className "api-snippet__code"
                $ S.text snippet
