module Timeline exposing (view)

import Axis exposing (tickCount)
import Har
import Html
import List.Extra as List
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Time exposing (posixToMillis)
import TypedSvg exposing (g, rect, style, svg)
import TypedSvg.Attributes exposing (class, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))


{-| Consolidated request data for graphing. We derive an id field from the
index of the request in the initial log to preserve uniqueness which is
required to correctly map the requests across a scale
-}
type alias ReqDatum =
    { id : Int
    , startTime : Float
    , endTime : Float
    }


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


view : Har.Log -> Svg msg
view log =
    let
        data =
            generateData log

        latestTime =
            List.map .endTime data
                |> List.maximumBy identity
                |> Maybe.withDefault 0

        xScale =
            Scale.band defaultBandConfig ( 0, 100 ) data

        yScale =
            Scale.linear ( 0, 100 ) ( 0, latestTime )
    in
    svg [ viewBox 0 0 100 100 ]
        [ style [] [ text """
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
          """ ]
        , g [ class [ "axis" ] ]
            [ Axis.right [ tickCount 10 ] yScale ]
        , g [ class [ "series" ] ]
            (data
                |> List.map (request xScale yScale)
            )
        ]


request : BandScale ReqDatum -> ContinuousScale Float -> ReqDatum -> Svg msg
request reqScale tScale datum =
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert reqScale datum
            , y <| Scale.convert tScale datum.startTime
            , width <| Scale.bandwidth reqScale
            , height <| Scale.convert tScale datum.endTime
            ]
            []
        ]
