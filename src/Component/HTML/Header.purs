-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Fpers.Component.HTML.Header where

import Prelude

import Fpers.Component.HTML.Utils (css, safeHref)
import Fpers.Data.Route (Route(..))
import Data.Monoid (guard)
import Halogen.HTML as HH

header :: forall i p. Route -> HH.HTML i p
header route =
  HH.nav
    [ css "navbar navbar-light" ]
    [ HH.div
      [ css "container" ]
      [ HH.a
        [ css "navbar-brand"
        , safeHref Home
        ]
        [ HH.text "conduit" ]
      , HH.ul
        [ css "nav navbar-nav pull-xs-right" ]
        [ navItem Home
            [ HH.text "Home" ]
        ]
      ]
    ]

  where
  navItem r html =
    HH.li
      [ css "nav-item" ]
      [ HH.a
        [ css $ "nav-link" <> guard (route == r) " active"
        , safeHref r
        ]
        html
      ]
