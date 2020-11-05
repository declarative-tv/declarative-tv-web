module Fpers.Page.Home where

import Prelude
import Data.Array (sortBy)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.String (replace, Pattern(..), Replacement(..))
import Effect.Aff.Class (class MonadAff)
import Fpers.Capability.Navigate (class Navigate)
import Fpers.Capability.Resource.Stream (class ManageStream, getStreams)
import Fpers.Capability.Resource.Streamer (class ManageStreamer, getStreamers)
import Fpers.Component.HTML.Footer (footer)
import Fpers.Component.HTML.Header (header)
import Fpers.Data.Route (Route(..))
import Fpers.Data.Stream (Stream)
import Fpers.Data.Streamer (Streamer)
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
  | LoadStreamers (Array String)

type State
  = { streamersInfo :: RemoteData String (Array StreamerInfo)
    , page :: Int
    }

type StreamerInfo
  = { streamer :: Streamer
    , stream :: Maybe Stream
    }

zipStreamers :: Array Streamer -> Array Stream -> Array StreamerInfo
zipStreamers streamers streams = map (go streams) streamers
  where
  go :: Array Stream -> Streamer -> StreamerInfo
  go ss streamer =
    { streamer
    , stream: find (\{ user_name } -> user_name == streamer.display_name) ss
    }

liveFirst :: StreamerInfo -> StreamerInfo -> Ordering
liveFirst a b = case a.stream, b.stream of
  Just _, Nothing -> LT
  Nothing, Just _ -> GT
  Just x, Just y -> if x.viewer_count > y.viewer_count then LT else GT
  _, _ -> EQ

component ::
  forall q o m.
  MonadAff m =>
  Navigate m =>
  ManageStream m =>
  ManageStreamer m =>
  H.Component HH.HTML q {} o m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  initialState _ =
    { streamersInfo: NotAsked
    , page: 1
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      let
        streamersNames =
          [ "CmdvTv"
          , "gillchristian"
          , "gernaderjake"
          , "baldbeardedbuilder"
          , "cassidoo"
          , "ifrostbolt"
          , "zkmushroom"
          , "rhymu8354"
          , "melkeydev"
          ]
      void $ H.fork $ handleAction $ LoadStreamers streamersNames
    LoadStreamers streamersNames -> do
      H.modify_ _ { streamersInfo = Loading }
      streamers <- getStreamers streamersNames
      streams <- getStreams streamersNames
      let
        streamersInfo = zipStreamers <$> streamers <*> streams
      H.modify_ _ { streamersInfo = fromMaybe (sortBy liveFirst <$> streamersInfo) }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { streamersInfo } =
    HH.div_
      [ header Home
      , HH.div
          [ HP.classes [ T.container ] ] case streamersInfo of
          NotAsked -> [ HH.text "..." ]
          Loading -> [ HH.text "..." ]
          Failure msg -> [ HH.text $ "Failed: " <> msg ]
          Success ss -> map streamerInfo ss
      , footer
      ]
    where
    streamerInfo :: StreamerInfo -> H.ComponentHTML Action slots m
    streamerInfo { streamer, stream } = case stream of
      Just s -> onlineStreamer streamer s
      Nothing -> offlineStreamer streamer

    offlineStreamer :: Streamer -> H.ComponentHTML Action slots m
    offlineStreamer { profile_image_url, display_name, login, description } =
      HH.div
        [ HP.classes [ T.mdFlex, T.m4 ] ]
        [ HH.div
            [ HP.classes [ T.mdFlexShrink0 ] ]
            [ HH.img
                [ HP.classes [ T.roundedFull, T.h16, T.w16, T.roundedMd ]
                , HP.src profile_image_url
                , HP.width 100
                , HP.height 100
                ]
            ]
        , HH.div
            [ HP.classes [ T.mt4, T.mdMt0, T.mdMl6 ] ]
            [ HH.div
                [ HP.classes [ T.trackingWide, T.textSm, T.textIndigo600, T.fontBold ] ]
                [ HH.text $ display_name ]
            , HH.div
                [ HP.classes [ T.block, T.mt1, T.textLg, T.leadingTight, T.fontSemibold, T.textGray900, T.hoverUnderline ] ]
                [ HH.text description ]
            ]
        ]

    onlineStreamer :: Streamer -> Stream -> H.ComponentHTML Action slots m
    onlineStreamer { profile_image_url } { title, user_name, viewer_count, thumbnail_url } =
      HH.div
        [ HP.classes [ T.mdFlex, T.m4 ] ]
        [ HH.div
            [ HP.classes [ T.mdFlexShrink0 ] ]
            [ HH.img [ HP.classes [ T.roundedLg, T.mdW56, T.roundedMd ], HP.src src, HP.width 440, HP.height 284 ]
            , HH.img
                [ HP.classes [ T.roundedFull, T.h16, T.w16, T.roundedMd ]
                , HP.src profile_image_url
                , HP.width 100
                , HP.height 100
                ]
            ]
        , HH.div
            [ HP.classes [ T.mt4, T.mdMt0, T.mdMl6 ] ]
            [ HH.div
                [ HP.classes [ T.trackingWide, T.textSm, T.textIndigo600, T.fontBold ] ]
                [ HH.text $ user_name <> " <> " <> show viewer_count ]
            , HH.div
                [ HP.classes [ T.block, T.mt1, T.textLg, T.leadingTight, T.fontSemibold, T.textGray900, T.hoverUnderline ] ]
                [ HH.text title ]
            ]
        ]
      where
      src =
        replace (Pattern "{width}") (Replacement "440")
          $ replace (Pattern "{height}") (Replacement "284")
          $ thumbnail_url
