module Component.Router where

import Prelude

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH

import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate)
import Component.ComponentA as ComponentA
import Component.ComponentB as ComponentB
import Data.Route (Route(..))

type State =
  { route :: Route }

data Query a
  = Navigate Route a

type Input =
  Maybe Route

type ChildQuery = Coproduct2
  ComponentA.Query
  ComponentB.Query

type ChildSlot = Either2
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
    { initialState: \initialRoute -> { route: fromMaybe ComponentA initialRoute }
    , render
    , eval
    , receiver: const Nothing
    }

  where

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (Navigate dest a) = do
    { route } <- H.get
    when (route /= dest) do
      H.modify_ _ { route = dest }
    pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } = case route of
    ComponentA ->
      HH.slot' CP.cp1 unit ComponentA.component unit absurd
    ComponentB ->
      HH.slot' CP.cp2 unit ComponentB.component unit absurd