module Fpers.Data.TwitchStreamer where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type TwitchStreamer
  = { login :: String
    , display_name :: String
    , description :: String
    , profile_image_url :: String
    }

twitchStreamerCodec :: JsonCodec TwitchStreamer
twitchStreamerCodec =
  CAR.object "TwitchStreamer"
    { login: CA.string
    , display_name: CA.string
    , description: CA.string
    , profile_image_url: CA.string
    }
