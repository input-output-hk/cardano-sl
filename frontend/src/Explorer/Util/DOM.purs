module Explorer.Util.DOM
    ( findElementById
    , scrollTop
    , targetToHTMLElement
    , targetToHTMLInputElement
    ) where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, HTMLInputElement, htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
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

findElementById :: forall eff. ElementId -> Eff (dom :: DOM | eff) (Maybe Element)
findElementById id' = do
    el <- window >>=
              document >>=
                  getElementById id' <<< htmlDocumentToNonElementParentNode
    pure $ toMaybe el
