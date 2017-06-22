module Explorer.View.Playground where

import Prelude
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.Util.Factory (mkCTxId)

import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML (div, h1) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!), (#!))

import Pux.DOM.HTML (HTML) as P

playgroundView :: State -> P.HTML Action
playgroundView state =
    S.div ! S.className "explorer-calculator"
          $ S.div ! S.className "explorer-calculator__container" $ do
              S.h1  ! S.className "headline"
                    #! P.onClick (const SocketCallMe)
                    $ S.text "socket -> callme"
              S.h1  ! S.className "headline"
                    #! P.onClick (const $ SocketCallMeString "hi there")
                    $ S.text "socket -> callme-string"
              S.h1  ! S.className "headline"
                    #! P.onClick (const <<< SocketCallMeCTxId $ mkCTxId "xyz-123-abc")
                    $ S.text "socket -> callme-txid"
