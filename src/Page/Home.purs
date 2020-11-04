module Fpers.Page.Home where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Fpers.Api.Endpoint (ArticleParams, Pagination, noArticleParams)
import Fpers.Capability.Navigate (class Navigate)
import Fpers.Capability.Resource.Article (class ManageArticle, getArticles, getCurrentUserFeed)
import Fpers.Capability.Resource.Stream (class ManageStream, getStreams)
import Fpers.Capability.Resource.Tag (class ManageTag, getAllTags)
import Fpers.Component.HTML.ArticleList (articleList, renderPagination)
import Fpers.Component.HTML.Footer (footer)
import Fpers.Component.HTML.Header (header)
import Fpers.Component.HTML.Utils (css, maybeElem, whenElem)
import Fpers.Component.Part.FavoriteButton (favorite, unfavorite)
import Fpers.Data.Article (ArticleWithMetadata)
import Fpers.Data.PaginatedArray (PaginatedArray)
import Fpers.Data.Profile (Profile)
import Fpers.Data.Route (Route(..))
import Fpers.Data.Stream (Stream)
import Fpers.Env (UserEnv)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | ShowTab Tab
  | LoadFeed Pagination
  | LoadArticles ArticleParams
  | LoadStreams (Array String)
  | LoadTags
  | FavoriteArticle Int
  | UnfavoriteArticle Int
  | SelectPage Int MouseEvent

type State =
  { tags :: RemoteData String (Array String)
  , articles :: RemoteData String (PaginatedArray ArticleWithMetadata)
  , streams :: RemoteData String (Array Stream)
  , tab :: Tab
  , page :: Int
  , currentUser :: Maybe Profile
  }

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

tabIsTag :: Tab -> Boolean
tabIsTag (Tag _) = true
tabIsTag _ = false

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => ManageTag m
  => ManageStream m
  => ManageArticle m
  => H.Component HH.HTML q {} o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser } =
    { tags: NotAsked
    , articles: NotAsked
    , streams: NotAsked
    , tab: Global
    , currentUser
    , page: 1
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadTags
      state <- H.get
      case state.currentUser of
        Nothing ->
          void $ H.fork $ handleAction $ LoadArticles noArticleParams
        profile -> do
          void $ H.fork $ handleAction $ LoadFeed { limit: Just 20, offset: Nothing }
          H.modify_ _ { tab = Feed }
      void $ H.fork $ handleAction $ LoadStreams ["CmdvTv", "gillchristian"]

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    LoadTags -> do
      H.modify_ _ { tags = Loading}
      tags <- getAllTags
      H.modify_ _ { tags = fromMaybe tags }

    LoadFeed params -> do
      st <- H.modify _ { articles = Loading }
      articles <- getCurrentUserFeed params
      H.modify_ _ { articles = fromMaybe articles }

    LoadArticles params -> do
      H.modify_ _ { articles = Loading }
      articles <- getArticles params
      H.modify_ _ { articles = fromMaybe articles }

    LoadStreams streamers -> do
      H.modify_ _ { streams = Loading }
      streams <- getStreams streamers
      H.modify_ _ { streams = fromMaybe streams }

    ShowTab thisTab -> do
      st <- H.get
      when (thisTab /= st.tab) do
        H.modify_ _ { tab = thisTab }
        void $ H.fork $ handleAction case thisTab of
          Feed ->
            LoadFeed { limit: Just 20, offset: Nothing }
          Global ->
            LoadArticles (noArticleParams { limit = Just 20 })
          Tag tag ->
            LoadArticles (noArticleParams { tag = Just tag, limit = Just 20 })

    FavoriteArticle index ->
      favorite (_article index)

    UnfavoriteArticle index ->
      unfavorite (_article index)

    SelectPage index event -> do
      H.liftEffect $ preventDefault $ toEvent event
      st <- H.modify _ { page = index }
      let offset = Just (index * 20)
      void $ H.fork $ handleAction case st.tab of
        Feed ->
          LoadFeed { limit: Just 20, offset }
        Global ->
          LoadArticles (noArticleParams { limit = Just 20, offset = offset })
        Tag tag ->
          LoadArticles (noArticleParams { tag = Just tag, limit = Just 20, offset = offset })

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state@{ tags, articles, currentUser } =
    HH.div_
      [ header currentUser Home
      , HH.div
          [ css "home-page" ]
          [ whenElem (isNothing currentUser) \_ -> banner
          , HH.div
              [ css "container page" ]
              [ HH.div
                  [ css "row" ]
                  [ mainView
                  , HH.div
                      [ css "col-md-3" ]
                      [ HH.div
                          [ css "sidebar" ]
                          [ HH.p_
                              [ HH.text "Popular Tags" ]
                          , renderTags tags
                          ]
                      ]
                  ]
              ]
          ]
      , footer
      ]
    where
    mainView =
      HH.div
        [ css "col-md-9" ]
        [ HH.div
            [ css "feed-toggle" ]
            [ HH.ul
              [ css "nav nav-pills outline-active" ]
              [ whenElem (isJust state.currentUser) \_ -> tab Feed
              , tab Global
              , whenElem (tabIsTag state.tab) \_ -> tab state.tab
              ]
            ]
        , articleList FavoriteArticle UnfavoriteArticle state.articles
        , maybeElem (toMaybe state.articles) \paginated ->
            renderPagination SelectPage state.page paginated
        ]

    banner =
      HH.div
        [ css "banner" ]
        [ HH.div
            [ css "container" ]
            [ HH.h1
                [ css "logo-font" ]
                [ HH.text "conduit" ]
            , HH.p_
                [ HH.text "A place to share your knowledge." ]
            ]
        ]

    tab thisTab =
      HH.li
        [ css "nav-item" ]
        [ HH.a
            [ css $ "nav-link" <> guard (state.tab == thisTab) " active"
            , HE.onClick \_ -> Just $ ShowTab thisTab
            , HP.href "#"
            ]
            case thisTab of
              Feed ->
                [ HH.text "Your Feed" ]
              Global ->
                [ HH.text "Global Feed" ]
              Tag tag ->
                [ HH.i
                  [ css "ion-pound" ]
                  []
                , HH.text $ "#" <> tag
                ]
        ]

    renderTags = case _ of
      NotAsked ->
        HH.div_
          [ HH.text "Tags not loaded" ]
      Loading ->
        HH.div_
          [ HH.text "Loading Tags" ]
      Failure err ->
        HH.div_
          [ HH.text $ "Failed loading tags: " <> err ]
      Success loadedTags ->
        HH.div
          [ css "tag-list" ]
          (map renderTag loadedTags)

    renderTag tag =
      HH.a
        [ css "tag-default tag-pill"
        , HE.onClick \_ -> Just $ ShowTab (Tag tag)
        , HP.href "#"
        ]
        [ HH.text tag ]

_article :: Int -> Traversal' State ArticleWithMetadata
_article i =
  prop (SProxy :: SProxy "articles")
    <<< _Success
    <<< prop (SProxy :: SProxy "body")
    <<< ix i
