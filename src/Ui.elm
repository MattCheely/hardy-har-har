module Ui exposing (color, fontSize, line)

import Color exposing (Color)


fontSize : Int
fontSize =
    16


{-| line height = 1.5
-}
line : Int
line =
    24


type alias Palette =
    { text : Color
    , background : Color
    , disabled : Color
    , interactive : Color
    , link : Color
    , buttonPrimary : Color
    , buttonSecondary : Color
    , buttonTertiary : Color
    }


color : Palette
color =
    { text = Color.rgb255 248 248 242
    , background = Color.rgb255 0 48 62
    , disabled = Color.rgb255 153 153 153
    , interactive = Color.rgb255 239 255 88
    , link = Color.rgb255 236 0 124
    , buttonPrimary = Color.rgb255 174 129 255
    , buttonSecondary = Color.rgb255 102 217 239
    , buttonTertiary = Color.rgb255 253 151 31
    }
