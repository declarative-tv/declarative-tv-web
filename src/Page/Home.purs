module Fpers.Page.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Fpers.Capability.Navigate (class Navigate)
import Fpers.Capability.Resource.Stream (class ManageStream, getStreams)
import Fpers.Component.HTML.Footer (footer)
import Fpers.Component.HTML.Header (header)
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
      let streamers = ["CmdvTv", "gillchristian", "gernaderjake", "baldbeardedbuilder", "ifrostbolt", "zkmushroom"]
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
          [ HP.classes [T.container] ]
          case streams of
            NotAsked -> [HH.text "..."]
            Loading -> [HH.text "..."]
            Failure msg -> [HH.text $ "Failed: " <> msg]
            Success ss -> map stream ss

      , footer
      ]
    where
      stream :: Stream -> H.ComponentHTML Action slots m
      stream { title, user_name, viewer_count } =
        HH.div
          [ HP.classes [T.mdFlex, T.m4]]
          [ HH.div
              [ HP.classes [T.mdFlexShrink0]]
              [ HH.img [HP.classes [T.roundedLg, T.mdW56], HP.src "", HP.width 448, HP.height 299 ] ]
          , HH.div
              [ HP.classes [T.mt4, T.mdMt0, T.mdMl6] ]
              [ HH.div
                  [ HP.classes [T.trackingWide, T.textSm, T.textIndigo600, T.fontBold ] ]
                  [ HH.text $ user_name <> " <> " <> show viewer_count  ]
              , HH.div
                  [ HP.classes [T.block, T.mt1, T.textLg, T.leadingTight, T.fontSemibold, T.textGray900, T.hoverUnderline] ]
                  [ HH.text title ]
              ]
          ]
