module Fpers.Capability.Resource.Streamer where

import Prelude
import Fpers.Data.Streamer (Streamer)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class
  Monad m <= ManageStreamer m where
  getStreamers :: m (Maybe (Array Streamer))

instance manageStreamersHalogenM ::
  ManageStreamer m =>
  ManageStreamer (HalogenM st act slots msg m) where
  getStreamers = lift getStreamers
