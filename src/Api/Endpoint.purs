module Fpers.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', prefix, root, string, many)
import Routing.Duplex.Generic (sum)
import Routing.Duplex.Generic.Syntax ((?))

data Endpoint
  = Streamers { user_login :: Array String }

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "helix" $ sum
  { "Streamers": "streams" ? { user_login: many <<< string } }
