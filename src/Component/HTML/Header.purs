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
            , T.py2
            ]
        ]
        [ HH.img
            [ HP.classes [ T.roundedMd, T.h12, T.w12, T.mr2 ]
            , HP.src "/assets/logo.png"
            ]
        , HH.h1
            [ HP.classes [ T.text2xl ] ]
            [ HH.text "Declarative Programming Streamers" ]
        ]
    ]
