module Main where

import Prelude
import Fpers.Api.Request (BaseURL(..))
import Fpers.AppM (runAppM)
import Fpers.Component.Router as Router
import Fpers.Data.Route (Route, routeCodec)
import Fpers.Env (Env, LogLevel(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Exception (throw)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Util as HU
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  HA.runHalogenAff do
    _ <- HA.awaitBody
    let
      baseUrl = BaseURL "https://fpers.vercel.app"

      logLevel = Dev

      streamersNames =
        [ "agentultra"
        , "avh4"
        , "chiroptical"
        , "cmdvtv"
        , "cvladfp"
        , "gillchristian"
        , "identitygs"
        , "kerckhove_ts"
        , "kritzcreek"
        , "quinndougherty92"
        , "totbwf"
        , "gernaderjake"
        , "lana_lux"
        ]
    nav <- liftEffect makeInterface
    let
      environment :: Env
      environment = { nav, baseUrl, logLevel, streamersNames }

      rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
      rootComponent = H.hoist (runAppM environment) Router.component
    mbEl <- HU.selectElement $ QuerySelector ".app"
    case mbEl of
      Just el -> do
        halogenIO <- runUI rootComponent {} el
        let
          onRouteChange :: Maybe Route -> Route -> Effect Unit
          onRouteChange old new =
            when (old /= Just new) do
              launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
        void $ liftEffect $ matchesWith (parse routeCodec) onRouteChange nav
      Nothing -> liftEffect $ throw "Could not mount app"
