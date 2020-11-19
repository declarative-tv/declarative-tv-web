module Fpers.AppM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Fpers.Api.Endpoint (Endpoint(..))
import Fpers.Api.Request (RequestMethod(..), RequestEndpoint(..))
import Fpers.Api.Utils (decode, mkAuthRequest)
import Fpers.Capability.LogMessages (class LogMessages)
import Fpers.Capability.Navigate (class Navigate, locationState)
import Fpers.Capability.Now (class Now)
import Fpers.Capability.Resource.Game (class ManageGame)
import Fpers.Capability.Resource.Stream (class ManageStream)
import Fpers.Capability.Resource.Streamer (class ManageStreamer)
import Fpers.Capability.Resource.TwitchStreamer (class ManageTwitchStreamer)
import Fpers.Data.Game (gameCodec)
import Fpers.Data.Log as Log
import Fpers.Data.Route as Route
import Fpers.Data.Stream (streamCodec)
import Fpers.Data.Streamer (streamerCodec)
import Fpers.Data.TwitchStreamer (twitchStreamerCodec)
import Fpers.Env (Env, LogLevel(..))
import Routing.Duplex (print)
import Type.Equality (class TypeEquals, from)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

-- TODO: log to a service on production
instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  locationState = liftEffect =<< _.nav.locationState <$> ask
  navigate route = do
    { pushState } <- asks _.nav
    { state } <- locationState
    liftEffect $ pushState state $ print Route.routeCodec $ route

instance manageStreamAppM :: ManageStream AppM where
  getStreams streamersNames = do
    mbJson <- mkAuthRequest { endpoint: API $ Streams { user_login: streamersNames }, method: Get }
    map (map _.data)
      $ decode (CAR.object "Streams" { "data": CAC.array streamCodec }) mbJson

instance manageStreamerAppM :: ManageStreamer AppM where
  getStreamers = do
    url <- asks _.streamersUrl
    mbJson <- mkAuthRequest { endpoint: External url, method: Get }
    decode (CAC.array streamerCodec) mbJson

instance manageTwitchStreamerAppM :: ManageTwitchStreamer AppM where
  getTwitchStreamers streamersNames = do
    mbJson <- mkAuthRequest { endpoint: API $ Streamers { login: streamersNames }, method: Get }
    map (map _.data)
      $ decode (CAR.object "TwitchStreamers" { "data": CAC.array twitchStreamerCodec }) mbJson

instance manageGameAppM :: ManageGame AppM where
  getGames gamesIds = do
    mbJson <- mkAuthRequest { endpoint: API $ Games { id: gamesIds }, method: Get }
    map (map _.data)
      $ decode (CAR.object "Games" { "data": CAC.array gameCodec }) mbJson
