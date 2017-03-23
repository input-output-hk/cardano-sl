module Explorer.View.Playground where

import Prelude
import Data.Generic (gShow)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.Util.Factory (mkCTxId)
import Pos.Explorer.Socket.Methods (Subscription(..))
import Pux.Html (Html, div, text, h1) as P
import Pux.Html.Attributes (className) as P
import Pux.Html.Events (onClick) as P

playgroundView :: State -> P.Html Action
playgroundView state =
    P.div
        [ P.className "explorer-calculator" ]
        [ P.div
            [ P.className "explorer-calculator__container" ]
            [ P.h1
                [ P.className "headline"
                , P.onClick $ const SocketCallMe ]
                [ P.text "socket -> callme" ]
            , P.h1
                [ P.className "headline"
                , P.onClick <<< const $ SocketCallMeString "hi there" ]
                [ P.text "socket -> callme-string" ]
            , P.h1
                [ P.className "headline"
                , P.onClick <<< const <<< SocketCallMeCTxId $ mkCTxId "xyz-123-abc" ]
                [ P.text "socket -> callme-txid" ]
            , P.h1
                [ P.className "headline"
                , P.onClick <<< const $ SocketSubscribe SubTx ]
                [ P.text $ "socket -> SocketSubscribe SubTx" ]
            ]
        ]
