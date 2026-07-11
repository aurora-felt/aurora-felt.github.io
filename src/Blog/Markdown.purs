module App.Markdown where

import Prelude

import CSS (maxWidth, pct, select)
import CSS.Selector (byClass, element, star, with, (|*))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (stylesheet)
import Halogen.HTML.Properties as HP
import MarkdownIt (Preset(..), html, newMarkdownIt)
import MarkdownIt as MarkdownIt
import Web.HTML (HTMLElement)


foreign import innerHTML :: HTMLElement -> String -> Effect Unit


data Action = Initialize

component :: forall q o m. MonadAff m => H.Component q String o m
component = do
  H.mkComponent
    { initialState: identity 
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                   , initialize = Just Initialize
                                   }

    }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM String Action () o m Unit
handleAction Initialize = do
  s <- H.get
  mdi <- H.liftEffect $ newMarkdownIt Default (html := true)
  md <- H.liftEffect $ MarkdownIt.render mdi s 
  ht <- H.getHTMLElementRef (H.RefLabel "markdown")
  flip traverse_ ht $ \el -> do
     H.liftEffect $ innerHTML el md

render :: forall s m . MonadAff m => s -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ stylesheet do
        select ((element "div" `with` byClass "markdown") |* star ) do
           maxWidth (pct 100.0)
    , HH.div
        [ HP.ref (H.RefLabel "markdown")
        , HP.classes [ClassName "markdown"]
        ]  []
    ]

