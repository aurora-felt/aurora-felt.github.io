module Blog.Header where

import Prelude

import Blog.SideBar (About(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

component :: forall q o . H.Component q About o Aff
component =
  H.mkComponent
    { initialState: identity 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

data Action = Initialize

handleAction :: forall m. Applicative m => Action -> m Unit 
handleAction _ = pure unit

type Slots :: forall k. Row k
type Slots = ()

render :: About -> H.ComponentHTML Action Slots Aff
render (About { header }) = HH.h1 [] [ HH.text header ] 


