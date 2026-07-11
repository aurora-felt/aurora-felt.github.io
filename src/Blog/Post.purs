module Blog.Post where

import Prelude

import Affjax.ResponseFormat (json, string)
import Affjax.Web (get)
import App.Markdown as Markdown
import CSS (alignItems, display, em, flex, flexDirection, fromHexString, height, justifyContent, padding, pct, position, relative, row, spaceBetween, width)
import CSS.Common (center)
import CSS.Font (color)
import CSS.Overflow (overflow, scroll, hidden)
import Color as Color
import Control.Monad.State (class MonadState)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Type.Proxy (Proxy(..))


newtype BlogPost =
  BlogPost {
    title :: String
  , date :: String
  , post :: String
  }

derive newtype instance DecodeJson BlogPost

getPostList :: Aff (Array BlogPost)
getPostList = either (const []) (\r -> either (const []) identity (decodeJson r.body)) <$> get json "/posts.json"

getPost :: BlogPost -> Aff String
getPost (BlogPost { post }) = either (const "Error...") (\r -> r.body) <$> get string ("/posts/" <> post)

component :: forall q o m. MonadAff m => H.Component q BlogPost o m
component = do
  H.mkComponent
    { initialState: \bp -> (bp /\ Nothing)
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                   , initialize = Just Initialize
                                   }
    }

data Action = Initialize

handleAction :: forall m. MonadAff m => MonadState (BlogPost /\ Maybe String) m => Action -> m Unit
handleAction Initialize = do
  (post /\ _) <- H.get
  md <- liftAff $ getPost post
  H.put (post /\ Just md)


type Slots = ( markdown :: forall m . H.Slot m String Unit )

_markdown = Proxy :: Proxy "markdown"

render :: forall a m . MonadAff m => BlogPost /\ Maybe String -> H.ComponentHTML a Slots m
render (BlogPost { title, date } /\ md) =
  HH.div
    [style do
        height (pct 100.0)
    ]
    [ HH.span
        [ style do
            display flex
            flexDirection row
            alignItems center
            justifyContent spaceBetween
        ]
        [ HH.div
            [ style do
                display flex
                flexDirection row
                alignItems center
            ]
            [ HH.div
                [ style do
                    width (em 1.0)
                ] []
            , HH.h2
                [ style do
                    color (maybe Color.black identity (fromHexString "#333333"))
                ]
                [ HH.text date
                ]
            , HH.div
                [ style do
                    width (em 1.0)
                ] []
            , HH.h2_
                [ HH.text title
                ]
            , HH.div
                [ style do
                    width (em 1.0)
                ] []
            ]
        , HH.div_ []
        ]
    , HH.div
        [ style do
            padding (em 0.0) (em 1.0) (em 0.0) (em 1.0)
        ]
        [ case md of
            Nothing -> HH.div_ []
            Just mk -> HH.slot_ _markdown unit Markdown.component mk
        ]
    ]
