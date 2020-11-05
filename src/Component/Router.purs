module Fpers.Component.Router where

import Prelude

import Fpers.Capability.LogMessages (class LogMessages)
import Fpers.Capability.Navigate (class Navigate, navigate, locationState)
import Fpers.Capability.Now (class Now)
import Fpers.Capability.Resource.Stream (class ManageStream)
import Fpers.Capability.Resource.Streamer (class ManageStreamer)
import Fpers.Component.Utils (OpaqueSlot)
import Fpers.Data.Route (Route(..), routeCodec)
import Fpers.Page.Home as Home
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD

type State = { route :: Maybe Route }

data Query a
  = Navigate Route a

data Action
  = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageStream m
  => ManageStreamer m
  => H.Component HH.HTML Query {} Void m
component = H.mkComponent
  { initialState: \_ -> { route: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> _.pathname <$> locationState
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home -> HH.slot (SProxy :: _ "home") unit Home.component {} absurd
    Nothing -> HH.div_ [ HH.text "Oh no! That page wasn't found." ]
