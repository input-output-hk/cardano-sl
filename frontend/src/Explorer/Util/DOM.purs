module Explorer.Util.DOM
    ( addClass
    , addClassToElement
    , classList
    , enterKeyPressed
    , eventToKeyPressed
    , findElementById
    , removeClass
    , removeClassFromElement
    , scrollTop
    , nodeToHTMLElement
    , nodeToHTMLInputElement
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, key)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, HTMLInputElement, htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (DOMTokenList, Element, ElementId, Node)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Pux.DOM.Events (DOMEvent)
import Unsafe.Coerce (unsafeCoerce)

foreign import scrollTopImpl :: forall eff. Eff (dom :: DOM | eff) Unit

-- | Helper function to scroll to top of a HTML page
scrollTop :: forall eff. Eff (dom :: DOM | eff) Unit
scrollTop = scrollTopImpl

-- | Converts a `Node` to `HTMLInputElement`
nodeToHTMLInputElement :: Node -> HTMLInputElement
nodeToHTMLInputElement = unsafeCoerce

-- | Converts a `Node` to `HTMLElement`
nodeToHTMLElement :: Node -> HTMLElement
nodeToHTMLElement = unsafeCoerce

-- | Helper function get an `getElementById` from document
findElementById :: forall eff. ElementId -> Eff (dom :: DOM | eff) (Maybe Element)
findElementById id' = do
    el <- window >>=
              document >>=
                  getElementById id' <<< htmlDocumentToNonElementParentNode
    pure el

-- | Returns a `classList` from `Element`
foreign import classList :: forall eff. Element -> Eff (dom :: DOM | eff) DOMTokenList


foreign import addClassImpl :: forall eff. EffFn2 (dom :: DOM | eff) DOMTokenList String Unit
-- | Adds a single `CSS` class to a `classList`
addClass :: forall eff. DOMTokenList -> String -> Eff (dom :: DOM | eff) Unit
addClass = runEffFn2 addClassImpl


foreign import removeClassImpl :: forall eff. EffFn2 (dom :: DOM | eff) DOMTokenList String Unit
-- | Removes a single `CSS` class from a `classList`
removeClass :: forall eff. DOMTokenList -> String -> Eff (dom :: DOM | eff) Unit
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

eventToKeyPressed :: DOMEvent -> String
eventToKeyPressed ev = either (const "") key $ runExcept $ eventToKeyboardEvent ev

enterKeyPressed :: DOMEvent -> Boolean
enterKeyPressed event =
    (eventToKeyPressed event) == "Enter"
