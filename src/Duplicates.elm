module Duplicates exposing (Model, Msg, init, update, view)

import Collection exposing (groupBy)
import Dict
import Har
import Html exposing (..)
import Html.Events exposing (onClick)
import List
import List.Extra as List



-- MODEL


type alias Model =
    { focused : Maybe String
    }


init : Model
init =
    { focused = Nothing }



-- UPDATE


type Msg
    = Focus String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Focus action ->
            { model | focused = Just action }



-- VIEW


view : Har.Log -> Model -> Html Msg
view log model =
    let
        counts =
            groupDuplicates log
    in
    case model.focused of
        Nothing ->
            tableView counts

        Just action ->
            Dict.fromList counts
                |> Dict.get action
                |> Maybe.map (detailView log action)
                |> Maybe.withDefault (tableView counts)


tableView : List ( String, List Har.Entry ) -> Html Msg
tableView counts =
    div []
        [ h1 [] [ text "Duplicated Requests" ]
        , table []
            (tr []
                [ th [] [ text "count" ], th [] [ text "Request URL" ] ]
                :: (counts
                        |> List.filter (Tuple.second >> List.length >> (<) 1)
                        |> List.map tableRow
                   )
            )
        ]


tableRow : ( String, List Har.Entry ) -> Html Msg
tableRow ( action, entries ) =
    tr []
        [ td [] [ text (String.fromInt (List.length entries)) ]
        , td [] [ a [ onClick (Focus action) ] [ text action ] ]
        ]


detailView : Har.Log -> String -> List Har.Entry -> Html Msg
detailView log action rawEntries =
    let
        entries =
            List.sortBy .time rawEntries
    in
    div []
        [ h1 [] [ text "Duplicates of ", text action ]
        , table []
            [ timeRow entries
            , pageRow log entries
            , statusRow entries
            ]
        ]


timeRow : List Har.Entry -> Html Msg
timeRow =
    labelledRow "time"
        (\entry ->
            span []
                [ text (String.fromFloat entry.time)
                , text "ms"
                ]
        )


pageRow : Har.Log -> List Har.Entry -> Html Msg
pageRow log =
    labelledRow "page"
        (\entry ->
            Har.page log entry
                |> Maybe.map (text << .title)
                |> Maybe.withDefault (text "")
        )


statusRow : List Har.Entry -> Html Msg
statusRow =
    labelledRow "status"
        (.response >> .status >> String.fromInt >> text)


labelledRow : String -> (a -> Html Msg) -> List a -> Html Msg
labelledRow title content items =
    tr []
        (th [] [ text title ]
            :: List.map (\item -> td [] [ content item ]) items
        )


groupDuplicates : Har.Log -> List ( String, List Har.Entry )
groupDuplicates log =
    log.entries
        |> groupBy (\entry -> entry.request.method ++ " " ++ entry.request.url)
        |> Dict.toList
        |> List.sortBy (Tuple.second >> List.length >> (-) 0)
