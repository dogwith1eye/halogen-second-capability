module Component.Home where

import Prelude

import Capability.LogMessages (class LogMessages)
import Component.Utils (safeHref)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH

data Query a = Initialize a

type State = { }

component :: forall m. LogMessages m => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const {}
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
    [ link ComponentA
    , link ComponentB ]

  link :: forall i p. Route -> HH.HTML i p
  link route =
    HH.div_
    [ HH.h1_
        [ HH.text $ show route ]
    , HH.p_
        [ HH.a
        [ safeHref route ]
        [ HH.text $ "Take me to " <> show route ]
      ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize next -> do
      pure next
