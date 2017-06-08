module Explorer.Util.DOM
    ( DOMClassList
    , addClass
    , addClassToElement
    , classList
    , findElementById
    , removeClass
    , removeClassFromElement
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
import Data.Function.Eff (EffFn2, runEffFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Pux.Html.Events (Target)
import Unsafe.Coerce (unsafeCoerce)

foreign import scrollTopImpl :: forall eff. Eff (dom :: DOM | eff) Unit

-- | Helper function to scroll to top of a HTML page
scrollTop :: forall eff. Eff (dom :: DOM | eff) Unit
scrollTop = scrollTopImpl

-- | Converts a Pux `Target` to DOM `HTMLInputElement`
targetToHTMLInputElement :: Target -> HTMLInputElement
targetToHTMLInputElement = unsafeCoerce

-- | Converts a Pux `Target` to DOM `HTMLElement`
targetToHTMLElement :: Target -> HTMLElement
targetToHTMLElement = unsafeCoerce

-- | Helper function get an `getElementById` from document
findElementById :: forall eff. ElementId -> Eff (dom :: DOM | eff) (Maybe Element)
findElementById id' = do
    el <- window >>=
              document >>=
                  getElementById id' <<< htmlDocumentToNonElementParentNode
    pure $ toMaybe el


-- | Data of `classList`
-- | see https://developer.mozilla.org/en-US/docs/Web/API/Element/classList
foreign import data DOMClassList :: *

-- | Returns a `classList` from `Element`
foreign import classList :: forall eff. Element -> Eff (dom :: DOM | eff) DOMClassList


foreign import addClassImpl :: forall eff. EffFn2 (dom :: DOM | eff) DOMClassList String Unit
-- | Adds a single `CSS` class to a `classList`
addClass :: forall eff. DOMClassList -> String -> Eff (dom :: DOM | eff) Unit
addClass = runEffFn2 addClassImpl


foreign import removeClassImpl :: forall eff. EffFn2 (dom :: DOM | eff) DOMClassList String Unit
-- | Removes a single `CSS` class from a `classList`
removeClass :: forall eff. DOMClassList -> String -> Eff (dom :: DOM | eff) Unit
removeClass = runEffFn2 removeClassImpl

-- | Helper function to add a single `CSS` class to a `classList` of an HTML element
addClassToElement :: forall eff. ElementId -> String -> Eff (dom :: DOM | eff) Unit
addClassToElement elemId clazz = do
    el <- findElementById elemId
    case el of
        Just el' -> do
            cL <- classList el'
            addClass cL clazz
        Nothing ->
            pure unit

-- | Helper function to remove a single `CSS` class from a `classList` of an HTML element
removeClassFromElement :: forall eff. ElementId -> String -> Eff (dom :: DOM | eff) Unit
removeClassFromElement elemId clazz = do
    el <- findElementById elemId
    case el of
        Just el' -> do
            cL <- classList el'
            removeClass cL clazz
        Nothing ->
            pure unit
