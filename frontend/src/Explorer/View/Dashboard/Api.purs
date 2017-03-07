module Explorer.View.Dashboard.Api (apiView) where

import Prelude
import Data.Lens ((^.))
import Data.Map (Map, fromFoldable, lookup, toAscUnfoldable) as M
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
import Pux.Html (Html, div, h3, text, p, code) as P
import Pux.Html.Attributes (className, dangerouslySetInnerHTML) as P
import Pux.Html.Events (onClick) as P

type ApiTabLabel = String

type ApiCode =
    { getAddress :: String
    , response :: String
    }

emptyApiCode :: ApiCode
emptyApiCode = { getAddress: "", response: ""}

apiCodes :: M.Map DashboardAPICode ApiCode
apiCodes =
  M.fromFoldable([
    Tuple Curl { getAddress: "Curl getAddress", response: "{\n\t\"hash\": }"}
    , Tuple Node { getAddress: "Node getAddress", response: "Node response ..."}
    , Tuple JQuery { getAddress: "jQuery getAddress", response: "jQuery response ..."}
    ])

apiView :: State -> P.Html Action
apiView state =
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ headerView state $ headerOptions lang'
          , P.div
            [ P.className "api-content" ]
            [ P.div
                [ P.className "api-content__container api-code"]
                [ P.div
                  [ P.className "api-code__tabs" ]
                  <<< map (apiCodeTabView state) <<< M.toAscUnfoldable $ apiTabs lang'
                , apiCodeSnippetView (translate (I18nL.dashboard <<< I18nL.dbGetAddress) lang') addressSnippet
                , apiCodeSnippetView (translate (I18nL.dashboard <<< I18nL.dbResponse) lang') responseSnippet
                ]
            , P.div
                [ P.className "api-content__container api-about"]
                [ P.h3
                    [ P.className "api-about__headline" ]
                    [ P.text $ translate (I18nL.dashboard <<< I18nL.dbAboutBlockchain) lang' ]
                , P.p
                    [ P.className "api-about__description"
                    , P.dangerouslySetInnerHTML $ translate (I18nL.dashboard <<< I18nL.dbAboutBlockchainDescription) lang' ]
                    []
                , P.div
                  [ P.className "api-about__button" ]
                  [ P.text $ translate (I18nL.dashboard <<< I18nL.dbGetApiKey) lang' ]
                ]
            ]
          ]
        ]
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

apiTabs :: Language -> M.Map DashboardAPICode ApiTabLabel
apiTabs lang =
    M.fromFoldable(
        [ Tuple Curl $ translate (I18nL.dashboard <<< I18nL.dbCurl) lang
        , Tuple Node $ translate (I18nL.dashboard <<< I18nL.dbNode) lang
        , Tuple JQuery $ translate (I18nL.dashboard <<< I18nL.dbJQuery) lang
        ])


apiCodeTabView :: State -> Tuple DashboardAPICode ApiTabLabel -> P.Html Action
apiCodeTabView state (Tuple code label) =
    P.div
      [ P.className $ "api-code__tab " <> selectedClazz
      , P.onClick <<< const $ DashboardShowAPICode code ]
      [ P.text label ]
    where
      selectedClazz = if state ^. dashboardSelectedApiCode == code then "selected" else ""


apiCodeSnippetView :: String -> String -> P.Html Action
apiCodeSnippetView headline snippet =
    P.div
        [ P.className "api-snippet" ]
        [ P.h3
            [ P.className "api-snippet__headline" ]
            [ P.text headline ]
        , P.code
            [ P.className "api-snippet__code" ]
            [ P.text snippet ]

        ]
