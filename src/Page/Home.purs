module Fpers.Page.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Fpers.Capability.Navigate (class Navigate)
import Fpers.Capability.Resource.Stream (class ManageStream, getStreams)
import Fpers.Component.HTML.Footer (footer)
import Fpers.Component.HTML.Header (header)
import Fpers.Component.HTML.Utils (css)
import Fpers.Data.Route (Route(..))
import Fpers.Data.Stream (Stream)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromMaybe)
import Tailwind as T

-- import Network.RemoteData (RemoteData(..), _Success, toMaybe)
-- import Web.Event.Event (preventDefault)
-- import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
  = Initialize
  | LoadStreams (Array String)

type State =
  { streams :: RemoteData String (Array Stream)
  , tab :: Tab
  , page :: Int
  }

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

tabIsTag :: Tab -> Boolean
tabIsTag (Tag _) = true
tabIsTag _ = false

component
  :: forall q o m
   . MonadAff m
  => Navigate m
  => ManageStream m
  => H.Component HH.HTML q {} o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState _ =
    { streams: NotAsked
    , tab: Global
    , page: 1
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      let streamers = ["CmdvTv", "gillchristian", "gernaderjake", "BaldBeardedBuilder"]
      void $ H.fork $ handleAction $ LoadStreams streamers

    LoadStreams streamers -> do
      H.modify_ _ { streams = Loading }
      streams <- getStreams streamers
      H.modify_ _ { streams = fromMaybe streams }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { streams } =
    HH.div_
      [ header Home
      , HH.div
          [ css "home-page" ]
          case streams of
            NotAsked -> [HH.text "..."]
            Loading -> [HH.text "..."]
            Failure msg -> [HH.text $ "Failed: " <> msg]
            Success ss -> map stream ss

      , footer
      ]
    where
      stream :: Stream -> H.ComponentHTML Action slots m
      stream { title, user_name } =
        HH.div
          [ HP.classes [T.m4] ]
          [ HH.text title
          , HH.br_
          , HH.text user_name
          , HH.hr_
          ]

