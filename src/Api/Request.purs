module Fpers.Api.Request
  ( Token -- constructor and decoders not exported
  , ClientId -- constructor and decoders not exported
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , readClientId
  , writeClientId
  , removeClientId
  , readToken
  , writeToken
  , removeToken
  ) where

import Prelude

import Affjax (Request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Fpers.Api.Endpoint (Endpoint, endpointCodec)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

newtype Token = Token String

-- | No `newtype` instance allowed! That would allow us to use the `wrap` and
-- | `unwrap` functions to access the string within the `Token` constructor.
derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token

-- | We won't derive a `Show` instance, either, because we don't ever want to
-- | reveal the token. Instead, we'll provide a manual instance.
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

newtype ClientId = ClientId String

-- | No `newtype` instance allowed! That would allow us to use the `wrap` and
-- | `unwrap` functions to access the string within the `ClientId` constructor.
derive instance eqClientId :: Eq ClientId
derive instance ordClientId :: Ord ClientId

-- | We won't derive a `Show` instance, either, because we don't ever want to
-- | reveal the client-id either. Instead, we'll provide a manual instance.
instance showClientId :: Show ClientId where
  show (ClientId _) = "ClientId {- client-id -}"

newtype BaseURL = BaseURL String

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> Maybe Token -> Maybe ClientId -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) token clientId { endpoint, method } =
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case token, clientId of
      Just (Token t), Just (ClientId id) ->
        [ RequestHeader "Authorization" $ "Bearer " <> t
        , RequestHeader "Client-Id" $ id
        ]
      _, _ -> []
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

-- | The following functions deal with writing, reading, and deleting tokens and
-- | client-id in local storage at a particular key. They'll be used as part of
-- | our production monad, `Fpers.AppM`.

tokenKey = "token" :: String

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) =
  setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken =
  removeItem tokenKey =<< localStorage =<< window

clientIdKey = "client_id" :: String

readClientId :: Effect (Maybe ClientId)
readClientId = do
  str <- getItem clientIdKey =<< localStorage =<< window
  pure $ map ClientId str

writeClientId :: ClientId -> Effect Unit
writeClientId (ClientId str) =
  setItem clientIdKey str =<< localStorage =<< window

removeClientId :: Effect Unit
removeClientId =
  removeItem clientIdKey =<< localStorage =<< window
