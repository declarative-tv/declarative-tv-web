module Fpers.Api.Request
  ( Token -- constructor and decoders not exported
  , ClientId -- constructor and decoders not exported
  , BaseURL(..)
  , RequestEndpoint(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  ) where

import Prelude

import Affjax (Request)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Fpers.Api.Endpoint (Endpoint, endpointCodec)
import Routing.Duplex (print)

newtype Token
  = Token String

-- | No `newtype` instance allowed! That would allow us to use the `wrap` and
-- | `unwrap` functions to access the string within the `Token` constructor.
derive instance eqToken :: Eq Token

derive instance ordToken :: Ord Token

-- | We won't derive a `Show` instance, either, because we don't ever want to
-- | reveal the token. Instead, we'll provide a manual instance.
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

newtype ClientId
  = ClientId String

-- | No `newtype` instance allowed! That would allow us to use the `wrap` and
-- | `unwrap` functions to access the string within the `ClientId` constructor.
derive instance eqClientId :: Eq ClientId

derive instance ordClientId :: Ord ClientId

-- | We won't derive a `Show` instance, either, because we don't ever want to
-- | reveal the client-id either. Instead, we'll provide a manual instance.
instance showClientId :: Show ClientId where
  show (ClientId _) = "ClientId {- client-id -}"

newtype BaseURL
  = BaseURL String

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

data RequestEndpoint
  = API Endpoint
  | External String

type RequestOptions
  = { endpoint :: RequestEndpoint
    , method :: RequestMethod
    }

defaultRequest :: BaseURL -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) { endpoint, method } =
  { method: Left method
  , url: url
  , headers: []
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  url = case endpoint of
    API e -> baseUrl <> print endpointCodec e
    External externalUrl -> externalUrl

  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing
