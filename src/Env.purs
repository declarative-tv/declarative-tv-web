module Fpers.Env where

import Prelude

import Fpers.Api.Request (BaseURL)
import Routing.PushState (PushStateInterface)

type Env =
  { nav :: PushStateInterface
  , logLevel :: LogLevel
  , baseUrl :: BaseURL
  , streamersUrl :: String
  }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel
