module Fpers.Capability.Resource.TwitchStreamer where

import Prelude
import Fpers.Data.TwitchStreamer (TwitchStreamer)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class
  Monad m <= ManageTwitchStreamer m where
  getTwitchStreamers :: Array String -> m (Maybe (Array TwitchStreamer))

instance manageTwitchStreamersHalogenM ::
  ManageTwitchStreamer m =>
  ManageTwitchStreamer (HalogenM st act slots msg m) where
  getTwitchStreamers = lift <<< getTwitchStreamers
