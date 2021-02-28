module Timeline exposing (view)

import Color exposing (Color)
import Har
import List.Extra as List
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Time exposing (posixToMillis)
import TypedSvg exposing (g, line, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, dominantBaseline, fill, stroke, strokeWidth, viewBox, x, x1, x2, y, y1, y2)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..), Transform(..), percent, px)
import Ui exposing (color)


{-| Consolidated request data for graphing. We derive an id field from the
index of the request in the initial log to preserve uniqueness which is
required to correctly map the requests across a scale
-}
type alias ReqDatum =
    { id : Int
    , startTime : Float
    , endTime : Float
    }


type alias Config =
    { pxPerSec : Int
    , topPadding : Float
    , textColor : Color
    , bgColor : Color
    , reqColor : Color
    , textOutline : Float
    , shortTickLen : Float
    , longTickLen : Float
    , tickTextSpacing : Float
    , majorGridColor : Color
    , minorGridColor : Color
    , pagePadding : Int
    }


config : Config
config =
    { pxPerSec = Ui.line * 6
    , topPadding = toFloat Ui.line
    , textColor = color.text
    , bgColor = color.background
    , reqColor = color.buttonTertiary
    , textOutline = 10
    , shortTickLen = 5
    , longTickLen = 10
    , tickTextSpacing = 5
    , majorGridColor = Color.rgba 0 0 0 0.4
    , minorGridColor = Color.rgba 0 0 0 0.1
    , pagePadding = 16 -- Set incorrectly as a kludge to deal with incomplete viewport tracking (probably related to scrollbars)
    }


view : Har.Log -> ( Int, Int ) -> Svg msg
view log ( winWidth, _ ) =
    let
        width =
            Debug.log "Width" winWidth

        data =
            generateData log

        latestTime =
            List.map .endTime data
                |> List.maximumBy identity
                |> Maybe.withDefault 0

        xMax =
            toFloat (winWidth - config.pagePadding * 2)

        yMax =
            toFloat config.pxPerSec * (latestTime / 1000)

        -- This is a pretty rough estimation
        axisWidth =
            config.longTickLen * 4

        xScale =
            Scale.band defaultBandConfig ( axisWidth, xMax ) data

        yScale =
            Scale.linear ( 0, yMax ) ( 0, latestTime )
    in
    svg [ viewBox 0 -config.topPadding xMax yMax, height yMax ]
        [ style [] [ text """
            .column rect { opacity: 0.8; }
            .column:hover rect { opacity: 1; }
          """ ]
        , msAxis yScale latestTime
        , g [ class [ "series" ] ]
            (data
                |> List.map (request xScale yScale)
            )
        ]


request : BandScale ReqDatum -> ContinuousScale Float -> ReqDatum -> Svg msg
request reqScale tScale datum =
    g [ class [ "column" ] ]
        [ rect
            [ x <| px (Scale.convert reqScale datum)
            , y <| px (Scale.convert tScale datum.startTime)
            , width <| Scale.bandwidth reqScale
            , height <| Scale.convert tScale datum.endTime
            , fill (Paint config.reqColor)
            ]
            []
        ]


msAxis : ContinuousScale Float -> Float -> Svg msg
msAxis scale maxTime =
    let
        majorTicks =
            List.range 0 (round (maxTime / 1000))

        fineTicks =
            majorTicks
                |> List.map (\sec -> List.map (\tenth -> sec * 1000 + tenth * 100) (List.range 1 9))
                |> List.concat
    in
    g [ class [ "axis" ] ]
        (List.concat
            [ List.map (majorTick scale) majorTicks
            , List.map (fineTick scale) fineTicks
            ]
        )


majorTick : ContinuousScale Float -> Int -> Svg msg
majorTick scale sec =
    let
        yPos =
            Scale.convert scale (toFloat (sec * 1000))

        tickLen =
            config.longTickLen
    in
    g
        [ class [ "major-tick" ] ]
        [ line
            [ class [ "grid-line" ]
            , x1 (px (tickLen + config.tickTextSpacing))
            , x2 (percent 100)
            , y1 (px yPos)
            , y2 (px yPos)
            , stroke (Paint config.majorGridColor)
            ]
            []
        , text_
            [ x (px (tickLen + config.tickTextSpacing))
            , y (px yPos)
            , dominantBaseline DominantBaselineMiddle
            , fill (Paint config.textColor)
            , stroke (Paint config.bgColor)
            , strokeWidth (px config.textOutline)
            , attribute "role" "presentation"
            ]
            [ text (String.fromInt sec)
            , text "s"
            ]
        , line
            [ x1 (px 0)
            , x2 (px tickLen)
            , y1 (px yPos)
            , y2 (px yPos)
            , stroke (Paint config.textColor)
            ]
            []
        , text_
            [ x (px (tickLen + config.tickTextSpacing))
            , y (px yPos)
            , dominantBaseline DominantBaselineMiddle
            , fill (Paint config.textColor)
            ]
            [ text (String.fromInt sec)
            , text "s"
            ]

        {- This requires some more math to position correctly for longer labels, and our
            estimate of character size might not be correct depending on the user's font
            configuration. It's fixable, but not worth the effort right now
           , line
               [ x1 (px (lineLen + 2 * textPad + textWidth))
               , x2 (px (2 * lineLen + 2 * textPad + textWidth))
               , y1 (px yPos)
               , y2 (px yPos)
               , stroke (Paint textColor)
               ]
               []
        -}
        ]


fineTick : ContinuousScale Float -> Int -> Svg msg
fineTick scale ms =
    let
        yPos =
            Scale.convert scale (toFloat ms)

        tickLen =
            if modBy 500 ms == 0 then
                config.longTickLen

            else
                config.shortTickLen
    in
    g [ class [ "fine-tick" ] ]
        [ line
            [ class [ "grid-line" ]
            , x1 (percent 0)
            , x2 (percent 100)
            , y1 (px yPos)
            , y2 (px yPos)
            , stroke (Paint config.minorGridColor)
            ]
            []
        , line
            [ x1 (px 0)
            , x2 (px tickLen)
            , y1 (px yPos)
            , y2 (px yPos)
            , stroke (Paint config.textColor)
            ]
            []
        ]


generateData : Har.Log -> List ReqDatum
generateData log =
    let
        zeroTime =
            List.map (.startedDateTime >> Time.posixToMillis) log.entries
                |> List.minimumBy identity
                |> Maybe.withDefault 0
    in
    List.indexedMap (entryToDatum zeroTime) log.entries


entryToDatum : Int -> Int -> Har.Entry -> ReqDatum
entryToDatum zeroTime idx entry =
    let
        startTime =
            posixToMillis entry.startedDateTime - zeroTime |> toFloat
    in
    { id = idx
    , startTime = startTime
    , endTime = startTime + entry.time
    }
