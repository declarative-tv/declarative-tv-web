module Fpers.Data.Stream where

import Data.Maybe (Maybe)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR

type Stream =
  { user_id :: String
  , user_name :: String
  , game_id :: String
  , title :: String
  , viewer_count :: Int
  , started_at :: String
  , thumbnail_url :: String
  , tag_ids :: Maybe (Array String)
  }

streamCodec :: JsonCodec Stream
streamCodec =
  CAR.object "Stream"
    { user_id: CA.string
    , user_name: CA.string
    , game_id: CA.string
    , title: CA.string
    , viewer_count: CA.int
    , started_at: CA.string
    , thumbnail_url: CA.string
    , tag_ids: CAC.maybe (CAC.array CA.string)
    }
