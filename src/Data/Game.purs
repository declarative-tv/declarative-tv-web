module Fpers.Data.Game where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type Game
  = { box_art_url :: String
    , id :: String
    , name :: String
    }

gameCodec :: JsonCodec Game
gameCodec =
  CAR.object "Game"
    { box_art_url: CA.string
    , id: CA.string
    , name: CA.string
    }
