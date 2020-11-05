module Fpers.Api.Utils where

import Prelude

import Affjax (request)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Fpers.Api.Request (BaseURL, RequestOptions, defaultRequest, readClientId, readToken)
import Fpers.Capability.LogMessages (class LogMessages, logError)
import Fpers.Capability.Now (class Now)

mkAuthRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
  { baseUrl } <- ask
  token <- liftEffect readToken
  clientId <- liftEffect readClientId
  response <- liftAff $ request $ defaultRequest baseUrl token clientId opts
  pure $ hush $ rmap _.body response

decode :: forall m a. LogMessages m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)
