module Fpers.Page.Home where

import Prelude
import Control.Parallel (parallel, sequential)
import Data.Array (sortBy, (:))
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (replace, Pattern(..), Replacement(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Fpers.Capability.Navigate (class Navigate)
import Fpers.Capability.Resource.Game (class ManageGame, getGames)
import Fpers.Capability.Resource.Stream (class ManageStream, getStreams)
import Fpers.Capability.Resource.Streamer (class ManageStreamer, getStreamers)
import Fpers.Capability.Resource.TwitchStreamer (class ManageTwitchStreamer, getTwitchStreamers)
import Fpers.Component.HTML.Header (header)
import Fpers.Data.Game (Game)
import Fpers.Data.Stream (Stream)
import Fpers.Data.TwitchStreamer (TwitchStreamer)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Tailwind as T

data Action
  = Initialize
  | LoadStreamers
  | LoadTwitchData (Array String)

type State
  = { streamersInfo :: RemoteData String (Array StreamerInfo)
    , games :: Array Game
    }

type StreamerInfo
  = { streamer :: TwitchStreamer
    , stream :: Maybe Stream
    }

zipStreamers :: Array TwitchStreamer -> Array Stream -> Array StreamerInfo
zipStreamers streamers streams = map go streamers
  where
  go :: TwitchStreamer -> StreamerInfo
  go streamer =
    { streamer
    , stream: find ((_ == streamer.display_name) <<< _.user_name) streams
    }

liveFirst :: StreamerInfo -> StreamerInfo -> Ordering
liveFirst a b = case a.stream, b.stream of
  Just _, Nothing -> LT
  Nothing, Just _ -> GT
  Just x, Just y -> if x.viewer_count > y.viewer_count then LT else GT
  _, _ -> compare a.streamer.login b.streamer.login

component ::
  forall q o m.
  Navigate m =>
  MonadAff m =>
  ManageGame m =>
  ManageStream m =>
  ManageStreamer m =>
  ManageTwitchStreamer m =>
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
  initialState _ = { streamersInfo: NotAsked, games: [] }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction LoadStreamers
    LoadStreamers -> do
      mbStreamers <- getStreamers
      case mbStreamers of
        Nothing -> H.modify_ _ { streamersInfo = Failure "Could not fetch streamers information" }
        Just streamers -> void $ H.fork $ handleAction $ LoadTwitchData (_.name <$> streamers)
    LoadTwitchData names -> do
      H.modify_ _ { streamersInfo = Loading }
      (streamers /\ streams) <-
        sequential $ (/\)
          <$> parallel (getTwitchStreamers names)
          <*> parallel (getStreams names)
      let
        mbStreamersInfo = sortBy liveFirst <$> (zipStreamers <$> streamers <*> streams)

        streamersInfo = maybe (Failure "Could not fetch streamers information") Success mbStreamersInfo
      H.modify_ _ { streamersInfo = streamersInfo }
      games <- case map _.game_id <$> streams of
        Just ids -> getGames ids
        Nothing -> pure Nothing
      H.modify_ _ { games = fromMaybe [] games }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { streamersInfo, games } =
    HH.div
      [ HP.classes [ T.minHScreen, T.bgGray200 ] ]
      [ header
      , HH.div
          [ HP.classes [ T.container, T.mxAuto, T.flex, T.flexCol, T.itemsCenter ] ]
          [ HH.div
              [ HP.classes
                  [ T.flex
                  , T.flexCol
                  , T.itemsCenter
                  , T.wFull
                  , T.maxW2xl
                  , T.mx2
                  , T.mt8
                  , T.p4
                  , T.shadowMd
                  , T.hoverShadowLg
                  , T.bgWhite
                  ]
              ]
              [ HH.div
                  [ HP.classes [ T.textGray800 ] ]
                  [ HH.text "We are a community of Functional and Declarative Programming streamers." ]
              , HH.div [ HP.classes [ T.flex ] ]
                  [ HH.a
                      [ HP.href "https://discord.gg/UMdeMUq"
                      , HP.classes [ T.flex, T.itemsCenter, T.textIndigo600, T.mt6, T.mr8 ]
                      ]
                      [ HH.img [ HP.src "/assets/discord.svg", HP.classes [ T.w16, T.mr2 ] ]
                      , HH.text "Join us on Discord"
                      ]
                  , HH.a
                      [ HP.href "https://github.com/chiroptical/declarative-programming-streams"
                      , HP.classes [ T.flex, T.itemsCenter, T.mt6 ]
                      ]
                      [ HH.img [ HP.src "/assets/github.png", HP.classes [ T.w12, T.mr2, T.textGray800 ] ]
                      , HH.text "List your stream"
                      ]
                  ]
              ]
          , HH.div [ HP.classes [ T.wFull, T.maxW2xl, T.mx2, T.mt8 ] ] feed
          ]
      ]
    where
    feed = case streamersInfo of
      Success ss -> map streamerInfo ss
      Failure msg ->
        [ HH.div
            [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
            [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
            , HH.p_ [ HH.text msg ]
            ]
        ]
      _ -> [ HH.div [ HP.classes [ T.textCenter ] ] [ HH.text "Loading ..." ] ]

    streamerInfo :: StreamerInfo -> H.ComponentHTML Action slots m
    streamerInfo { streamer, stream } = case stream of
      Just s -> onlineStreamer streamer s
      Nothing -> offlineStreamer streamer

    offlineStreamer :: TwitchStreamer -> H.ComponentHTML Action slots m
    offlineStreamer { profile_image_url, display_name, login, description } =
      HH.div
        [ HP.classes [ T.mb6, T.p4, T.shadowMd, T.hoverShadowLg, T.bgWhite, T.grid, T.gridCols10, T.gap2 ] ]
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

    onlineStreamer :: TwitchStreamer -> Stream -> H.ComponentHTML Action slots m
    onlineStreamer { profile_image_url } { game_id, title, user_name, viewer_count, thumbnail_url } =
      HH.div
        [ HP.classes [ T.mb6, T.p4, T.shadowMd, T.hoverShadowLg, T.bgWhite, T.flex, T.flexCol ] ]
        [ HH.img [ HP.classes [ T.wFull ], HP.src thumbnail, HP.width 632, HP.height 350 ]
        , HH.div
            [ HP.classes [ T.mt6, T.grid, T.gridCols10, T.gap2 ] ]
            [ HH.a
                [ HP.href $ "https://twitch.tv/" <> user_name, HP.classes [ T.colSpan1 ] ]
                [ HH.img [ HP.classes [ T.roundedFull, T.h12, T.w12 ], HP.src profile_image_url ]
                ]
            , HH.div [ HP.classes [ T.colSpan9 ] ]
                [ HH.div
                    [ HP.classes [ T.flex ] ]
                    $ ( HH.a
                          [ HP.href $ "https://twitch.tv/" <> user_name
                          , HP.classes
                              [ T.flex
                              , T.itemsCenter
                              , T.block
                              , T.textIndigo700
                              , T.textLg
                              , T.fontBold
                              , T.leadingNone
                              , T.hoverUnderline
                              ]
                          ]
                          [ HH.span [ HP.classes [ T.relative, T.flex, T.h3, T.w3, T.mr2 ] ]
                              [ HH.span [ HP.classes [ T.animatePing, T.absolute, T.inlineFlex, T.wFull, T.hFull, T.roundedFull, T.bgRed700, T.opacity75 ] ] []
                              , HH.span [ HP.classes [ T.relative, T.inlineFlex, T.roundedFull, T.w3, T.h3, T.bgRed600 ] ] []
                              ]
                          , HH.text user_name
                          ]
                      )
                    : case mbGame of
                        Nothing -> []
                        Just { name } ->
                          [ HH.div [ HP.classes [ T.mx2, T.leadingNone, T.textLg ] ] [ HH.text "•" ]
                          , HH.a
                              [ HP.classes
                                  [ T.block
                                  , T.textIndigo600
                                  , T.textLg
                                  , T.leadingNone
                                  , T.hoverUnderline
                                  ]
                              , HP.href $ "https://twitch.tv/directory/game/" <> name
                              ]
                              [ HH.text name ]
                          ]
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
      mbGame = find ((_ == game_id) <<< _.id) games

      thumbnail =
        replace (Pattern "{width}") (Replacement "632")
          $ replace (Pattern "{height}") (Replacement "350")
          $ thumbnail_url
