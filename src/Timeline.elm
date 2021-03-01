module Timeline exposing (Model, Msg, init, update, view)

import Color exposing (Color)
import Har
import List.Extra as List
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Time exposing (posixToMillis)
import TypedSvg exposing (g, line, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, dominantBaseline, fill, height, stroke, strokeWidth, textAnchor, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core as Svg exposing (Svg, attribute, text)
import TypedSvg.Events exposing (onMouseEnter, onMouseLeave)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..), Transform(..), percent, px)
import Ui exposing (color)


type alias Model =
    { focusedRequest : Maybe Int
    }


init : Model
init =
    { focusedRequest = Nothing }


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
    , yPadding : Float
    , textColor : Color
    , bgColor : Color
    , reqColor : Color
    , reqHighlightColor : Color
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
    , yPadding = toFloat Ui.line
    , textColor = color.text
    , bgColor = color.background
    , reqColor = color.buttonPrimary
    , reqHighlightColor = color.buttonTertiary
    , textOutline = 10
    , shortTickLen = 5
    , longTickLen = 10
    , tickTextSpacing = 5
    , majorGridColor = Color.rgba 0 0 0 0.4
    , minorGridColor = Color.rgba 0 0 0 0.1
    , pagePadding = 16 -- Set incorrectly as a kludge to deal with incomplete viewport tracking (probably related to scrollbars or aspect ratio scaling)
    }



-- UPDATE


type Msg
    = MouseEnterRequest Int
    | MouseLeaveRequest


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseEnterRequest id ->
            { model | focusedRequest = Just id }

        MouseLeaveRequest ->
            { model | focusedRequest = Nothing }



-- VIEW


view : Har.Log -> ( Int, Int ) -> Model -> Svg Msg
view log ( winWidth, _ ) model =
    let
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
    svg
        [ viewBox 0 -config.yPadding xMax (yMax + 2 * config.yPadding)
        , height (px (yMax + 2 * config.yPadding))
        ]
        [ style [] [ text """
            .column rect { opacity: 0.8; }
            .column:hover rect { opacity: 1; }
          """ ]
        , msAxis yScale latestTime
        , g [ class [ "series" ] ]
            (data
                |> List.map (request xScale yScale model.focusedRequest)
            )
        ]


request : BandScale ReqDatum -> ContinuousScale Float -> Maybe Int -> ReqDatum -> Svg Msg
request reqScale tScale focusedRequest datum =
    let
        focused =
            Just datum.id == focusedRequest

        fillColor =
            if focused then
                config.reqHighlightColor

            else
                config.reqColor
    in
    g
        [ class [ "column" ]
        , onMouseEnter (MouseEnterRequest datum.id)
        , onMouseLeave MouseLeaveRequest
        ]
        (rect
            [ x (px (Scale.convert reqScale datum))
            , y (px 0)
            , width (px (Scale.bandwidth reqScale))
            , height (percent 100)
            , fill (Paint (Color.rgba 0 0 0 0))
            ]
            []
            :: rect
                [ x (px (Scale.convert reqScale datum))
                , y (px (Scale.convert tScale datum.startTime))
                , width (px (Scale.bandwidth reqScale))
                , height (px (max 2 (Scale.convert tScale (datum.endTime - datum.startTime))))
                , fill (Paint fillColor)
                ]
                []
            :: (if focused then
                    focusedDecorators reqScale tScale datum

                else
                    []
               )
        )


focusedDecorators : BandScale ReqDatum -> ContinuousScale Float -> ReqDatum -> List (Svg Msg)
focusedDecorators reqScale tScale datum =
    let
        startPos =
            Scale.convert tScale datum.startTime

        endPos =
            Scale.convert tScale datum.endTime

        midPos =
            Scale.convert tScale ((datum.startTime + datum.endTime) / 2)

        reqEdge =
            Scale.convert reqScale datum

        markColor =
            config.reqHighlightColor

        height =
            endPos - startPos

        ( startAlign, endAlign ) =
            if height < toFloat Ui.fontSize then
                ( DominantBaselineAuto, DominantBaselineHanging )

            else
                ( DominantBaselineMiddle, DominantBaselineMiddle )

        textOffset =
            if height < config.textOutline then
                config.textOutline / 2

            else
                0
    in
    [ line
        [ class [ "req-mark" ]
        , x1 (px 0)
        , x2 (px (reqEdge - config.tickTextSpacing))
        , y1 (px startPos)
        , y2 (px startPos)
        , stroke (Paint markColor)
        ]
        []
    , outlinedText
        [ class [ "req-start-time" ]
        , x (px (config.longTickLen * 2 + config.tickTextSpacing))
        , y (px (startPos - textOffset))
        , dominantBaseline startAlign
        , fill (Paint markColor)
        ]
        [ text (timeLabel datum.startTime) ]
    , line
        [ class [ "req-mark" ]
        , x1 (px 0)
        , x2 (px (reqEdge - config.tickTextSpacing))
        , y1 (px endPos)
        , y2 (px endPos)
        , stroke (Paint markColor)
        ]
        []
    , outlinedText
        [ class [ "req-end-time" ]
        , x (px (config.longTickLen * 2 + config.tickTextSpacing))
        , y (px (endPos + textOffset))
        , dominantBaseline endAlign
        , fill (Paint markColor)
        ]
        [ text (timeLabel datum.endTime) ]
    , outlinedText
        [ class [ "req-duration" ]
        , x (px (reqEdge - config.tickTextSpacing))
        , y (px midPos)
        , textAnchor AnchorEnd
        , dominantBaseline DominantBaselineMiddle
        , fill (Paint markColor)
        ]
        [ text (timeLabel (datum.endTime - datum.startTime)) ]
    ]


msAxis : ContinuousScale Float -> Float -> Svg Msg
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


majorTick : ContinuousScale Float -> Int -> Svg Msg
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
        , line
            [ x1 (px 0)
            , x2 (px tickLen)
            , y1 (px yPos)
            , y2 (px yPos)
            , stroke (Paint config.textColor)
            ]
            []
        , outlinedText
            [ x (px (tickLen + config.tickTextSpacing))
            , y (px yPos)
            , dominantBaseline DominantBaselineMiddle
            , fill (Paint config.textColor)
            ]
            [ text (timeLabel (toFloat (sec * 1000)))
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


outlinedText : List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
outlinedText attrs content =
    g []
        [ text_
            (List.concat
                [ attrs
                , [ stroke (Paint config.bgColor)
                  , strokeWidth (px config.textOutline)
                  , attribute "role" "presentation"
                  ]
                ]
            )
            content
        , text_ attrs content
        ]


timeLabel : Float -> String
timeLabel ms =
    let
        secs =
            truncate (ms / 1000)

        millis =
            ms - toFloat (secs * 1000)
    in
    if millis < 0.5 then
        String.fromInt secs ++ "s"

    else if secs == 0 then
        if ms >= 100 then
            (String.fromFloat ms
                |> String.left 3
            )
                ++ "ms"

        else
            (String.fromFloat ms
                |> String.left 4
            )
                ++ "ms"

    else
        (String.fromFloat (ms / 1000)
            |> String.left 5
        )
            ++ "s"


fineTick : ContinuousScale Float -> Int -> Svg Msg
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
