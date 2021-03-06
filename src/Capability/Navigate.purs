module Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Route (Route)
import Halogen (HalogenM)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM s f g p o m) where
  navigate = lift <<< navigate