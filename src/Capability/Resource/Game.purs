module Fpers.Capability.Resource.Game where

import Prelude

import Fpers.Data.Game (Game)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageGame m where
  getGames :: Array String -> m (Maybe (Array Game))

instance manageGameHalogenM :: ManageGame m => ManageGame (HalogenM st act slots msg m) where
  getGames = lift <<< getGames
