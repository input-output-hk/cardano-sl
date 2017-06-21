module Explorer.View.Playground where

import Prelude
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.Util.Factory (mkCTxId)

import Text.Smolder.HTML (div, text, h1)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (#!))

import Pux.DOM.HTML (HTML) as P

playgroundView :: State -> P.HTML Action
playgroundView state =
    div ! className "explorer-calculator" $ do
        div ! className "explorer-calculator__container" $ do
            h1  ! className "headline"
                #! onClick (const SocketCallMe)
                $ text "socket -> callme"
            h1  ! className "headline"
                #! onClick (const $ SocketCallMeString "hi there")
                $ text "socket -> callme-string"
            h1  ! className "headline"
                #! onClick (const <<< SocketCallMeCTxId $ mkCTxId "xyz-123-abc")
                $ text "socket -> callme-txid"
