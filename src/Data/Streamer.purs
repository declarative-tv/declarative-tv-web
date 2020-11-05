module Fpers.Data.Streamer where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type Streamer
  = { login :: String
    , display_name :: String
    , description :: String
    , profile_image_url :: String
    }

streamerCodec :: JsonCodec Streamer
streamerCodec =
  CAR.object "Streamer"
    { login: CA.string
    , display_name: CA.string
    , description: CA.string
    , profile_image_url: CA.string
    }
