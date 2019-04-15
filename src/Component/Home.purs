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
      [ HH.h1_
          [ HH.text "B" ]
      , HH.p_
          [ HH.a
          [ safeHref ComponentB ]
          [ HH.text "Already have an account?" ]
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize next -> do
      pure next
