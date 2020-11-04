module Fpers.Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Fpers.Data.Route (Route)
import Halogen (HalogenM)
import Routing.PushState (LocationState)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  locationState :: m LocationState

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  locationState = lift locationState
