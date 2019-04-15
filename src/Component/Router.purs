module Component.Router where

import Prelude

import Capability.LogMessages (class LogMessages, log)
import Capability.Navigate (class Navigate)
import Component.ComponentA as ComponentA
import Component.ComponentB as ComponentB
import Component.Home as Home
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH

type State =
  { route :: Route }

data Query a
  = Navigate Route a

type Input =
  Maybe Route

type ChildQuery = Coproduct3
  Home.Query
  ComponentA.Query
  ComponentB.Query

type ChildSlot = Either3
  Unit
  Unit
  Unit

component
  :: forall m
   . MonadAff m
  => LogMessages m
  => Navigate m
  => H.Component HH.HTML Query Input Void m
component =
  H.parentComponent
    { initialState: \initialRoute -> { route: fromMaybe Home initialRoute }
    , render
    , eval
    , receiver: const Nothing
    }

  where

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (Navigate dest a) = do
    { route } <- H.get
    log route
    when (route /= dest) do
      H.modify_ _ { route = dest }
    pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } = case route of
    Home ->
      HH.slot' CP.cp1 unit Home.component unit absurd
    ComponentA ->
      HH.slot' CP.cp2 unit ComponentA.component unit absurd
    ComponentB ->
      HH.slot' CP.cp3 unit ComponentB.component unit absurd