module Fpers.Api.Endpoint where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', prefix, root, string, many)
import Routing.Duplex.Generic (sum)
import Routing.Duplex.Generic.Syntax ((?))

data Endpoint
  = Streams { user_login :: Array String }
  | Streamers { login :: Array String }

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root $ prefix "api"
    $ sum
        { "Streams": "streams" ? { user_login: many <<< string }
        , "Streamers": "streamers" ? { login: many <<< string }
        }
