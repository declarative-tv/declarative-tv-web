module Fpers.Component.HTML.Header where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tailwind as T

header :: forall i p. HH.HTML i p
header =
  HH.nav
    []
    [ HH.div
        [ HP.classes
            [ T.container
            , T.mxAuto
            , T.flex
            , T.justifyCenter
            , T.itemsCenter
            , T.pt10
            , T.pb2
            , T.textGray800
            ]
        ]
        [ HH.img
            [ HP.classes [ T.roundedMd, T.h12, T.w12, T.mr2 ]
            , HP.src "/assets/logo.png"
            ]
        , HH.h1
            [ HP.classes [ T.text3xl, T.fontBlack, T.fontSans ] ]
            [ HH.text "Declarative Programming Streamers" ]
        ]
    ]
