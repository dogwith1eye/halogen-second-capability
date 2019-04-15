module Component.Utils where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

import Data.Route (Route, routeCodec)

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec
