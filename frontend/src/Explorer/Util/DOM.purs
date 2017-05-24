module Explorer.Util.DOM
    ( scrollTop
    , targetToHTMLElement
    , targetToHTMLInputElement
    ) where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement, HTMLInputElement)
import Pux.Html.Events (Target)
import Unsafe.Coerce (unsafeCoerce)

foreign import scrollTopImpl :: forall eff. Eff (dom :: DOM | eff) Unit

scrollTop :: forall eff. Eff (dom :: DOM | eff) Unit
scrollTop = scrollTopImpl

-- Converts a Pux `Target` to DOM `HTMLInputElement`
targetToHTMLInputElement :: Target -> HTMLInputElement
targetToHTMLInputElement = unsafeCoerce

-- Converts a Pux `Target` to DOM `HTMLElement`
targetToHTMLElement :: Target -> HTMLElement
targetToHTMLElement = unsafeCoerce
