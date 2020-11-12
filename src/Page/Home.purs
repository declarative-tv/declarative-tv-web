module Fpers.Page.Home where

import Prelude

import Data.Array (sortBy)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), maybe)
import Data.String (replace, Pattern(..), Replacement(..))
import Effect.Aff.Class (class MonadAff)
import Fpers.Capability.Navigate (class Navigate)
import Fpers.Capability.Resource.Stream (class ManageStream, getStreams)
import Fpers.Capability.Resource.Streamer (class ManageStreamer, getStreamers)
import Fpers.Component.HTML.Header (header)
import Fpers.Data.Stream (Stream)
import Fpers.Data.Streamer (Streamer)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Tailwind as T

-- import Fpers.Data.Route (Route(..))
-- import Fpers.Component.HTML.Footer (footer)
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
  _, _ -> compare a.streamer.login b.streamer.login

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
          [ "agentultra"
          , "avh4"
          , "chiroptical"
          , "cmdvtv"
          , "cvladfp"
          , "gillchristian"
          , "identitygs"
          , "kerckhove_ts"
          , "quinndougherty92"
          , "totbwf"
          , "gernaderjake"
          ]
      void $ H.fork $ handleAction $ LoadStreamers streamersNames
    LoadStreamers streamersNames -> do
      H.modify_ _ { streamersInfo = Loading }
      streamers <- getStreamers streamersNames
      streams <- getStreams streamersNames
      let
        mbStreamersInfo = sortBy liveFirst <$> (zipStreamers <$> streamers <*> streams)

        streamersInfo = maybe (Failure "Could not fetch streamers information") Success mbStreamersInfo
      H.modify_ _ { streamersInfo = streamersInfo }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { streamersInfo } =
    HH.div_
      [ header
      , HH.div
          [ HP.classes [ T.container, T.mxAuto, T.flex, T.flexCol, T.itemsCenter ] ]
          [ HH.div [ HP.classes [ T.wFull, T.maxW2xl, T.mx2 ] ] feed ]
      ]
    where
    feed = case streamersInfo of
      NotAsked -> [ HH.text "Loading ..." ]
      Loading -> [ HH.text "Loading ..." ]
      Failure msg ->
        [ HH.div
            [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
            [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
            , HH.p_ [ HH.text msg ]
            ]
        ]
      Success ss -> map streamerInfo ss

    streamerInfo :: StreamerInfo -> H.ComponentHTML Action slots m
    streamerInfo { streamer, stream } = case stream of
      Just s -> onlineStreamer streamer s
      Nothing -> offlineStreamer streamer

    offlineStreamer :: Streamer -> H.ComponentHTML Action slots m
    offlineStreamer { profile_image_url, display_name, login, description } =
      HH.div
        [ HP.classes [ T.my6, T.p4, T.border4, T.grid, T.gridCols10, T.gap2 ] ]
        [ HH.a
            [ HP.href $ "https://twitch.tv/" <> login, HP.classes [ T.colSpan1 ] ]
            [ HH.img
                [ HP.classes [ T.roundedFull, T.h12, T.w12 ]
                , HP.src profile_image_url
                ]
            ]
        , HH.div [ HP.classes [ T.colSpan9 ] ]
            [ HH.a
                [ HP.href $ "https://twitch.tv/" <> login
                , HP.classes
                    [ T.flex
                    , T.itemsCenter
                    , T.textIndigo700
                    , T.fontBold
                    , T.block
                    , T.textLg
                    , T.leadingNone
                    , T.fontSemibold
                    , T.hoverUnderline
                    ]
                ]
                [ HH.div [ HP.classes [ T.bgGray600, T.w3, T.h3, T.roundedFull, T.mr1 ] ] [], HH.text display_name ]
            , HH.p [ HP.classes [ T.textSm, T.textGray700, T.breakNormal, T.mt2 ] ] [ HH.text description ]
            ]
        ]

    onlineStreamer :: Streamer -> Stream -> H.ComponentHTML Action slots m
    onlineStreamer { profile_image_url } { title, user_name, viewer_count, thumbnail_url } =
      HH.div
        [ HP.classes [ T.my6, T.p4, T.border4, T.flex, T.flexCol ] ]
        [ HH.img [ HP.classes [ T.wFull ], HP.src src, HP.width 632, HP.height 350 ]
        , HH.div
            [ HP.classes [ T.mt6, T.grid, T.gridCols10, T.gap2 ] ]
            [ HH.a
                [ HP.href $ "https://twitch.tv/" <> user_name, HP.classes [ T.colSpan1 ] ]
                [ HH.img [ HP.classes [ T.roundedFull, T.h12, T.w12 ], HP.src profile_image_url ]
                ]
            , HH.div [ HP.classes [ T.colSpan9 ] ]
                [ HH.a
                    [ HP.href $ "https://twitch.tv/" <> user_name
                    , HP.classes
                        [ T.flex
                        , T.itemsCenter
                        , T.textIndigo700
                        , T.fontBold
                        , T.block
                        , T.textLg
                        , T.leadingNone
                        , T.fontSemibold
                        , T.textGray900
                        , T.hoverUnderline
                        ]
                    ]
                    [ HH.div [ HP.classes [ T.bgRed600, T.w3, T.h3, T.roundedFull, T.mr1 ] ] [], HH.text user_name ]
                , HH.a
                    [ HP.href $ "https://twitch.tv/" <> user_name
                    , HP.classes
                        [ T.block
                        , T.textLg
                        , T.leadingNone
                        , T.fontSemibold
                        , T.textGray900
                        , T.hoverUnderline
                        , T.mt2
                        ]
                    ]
                    [ HH.text title ]
                ]
            ]
        ]
      where
      src =
        replace (Pattern "{width}") (Replacement "632")
          $ replace (Pattern "{height}") (Replacement "350")
          $ thumbnail_url
