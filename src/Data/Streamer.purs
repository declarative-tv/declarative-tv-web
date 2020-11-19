module Fpers.Data.Streamer where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Compat as CAC

type Streamer
  = { name :: String
    , channel :: String
    , speaking :: Array String
    , languages :: Array String
    , schedule :: String
    }

streamerCodec :: JsonCodec Streamer
streamerCodec =
  CAR.object "Streamer"
    { name: CA.string
    , channel: CA.string
    , speaking: CAC.array CA.string
    , languages: CAC.array CA.string
    , schedule: CA.string
    }
