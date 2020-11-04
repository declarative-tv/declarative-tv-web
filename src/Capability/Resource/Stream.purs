module Fpers.Capability.Resource.Stream where

import Prelude

import Fpers.Data.Stream (Stream)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageStream m where
  getStreams :: Array String -> m (Maybe (Array Stream))

instance manageStreamHalogenM :: ManageStream m => ManageStream (HalogenM st act slots msg m) where
  getStreams = lift <<< getStreams
