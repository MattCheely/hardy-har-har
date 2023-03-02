module Cors exposing (Model, Msg, init, update, view)

import Har
import Html exposing (Html, a, div, h1, h2, h3, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Iso8601
import List.Extra as ListX
import Set exposing (Set)



-- MODEL


type alias Model =
    { view : View }


init : Model
init =
    { view = Summary }


type View
    = Summary
    | NoAllowedHeader Har.Entry
    | NotAllowedOrigin Har.Entry



-- UPDATE


type Msg
    = ChangeView View


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeView newView ->
            { model | view = newView }



-- View


view : Har.Log -> Model -> Html Msg
view log model =
    case model.view of
        Summary ->
            summaryView log

        NoAllowedHeader entry ->
            noAllowedHeaderView entry

        NotAllowedOrigin entry ->
            notAllowedOriginView entry


summaryView : Har.Log -> Html Msg
summaryView log =
    let
        errorRequests =
            corsFailures log
    in
    div []
        [ h1 [] [ text "Requests that would fail CORS policy" ]
        , noAllowedOriginHeaderList errorRequests.noAllowedOriginHeader
        , notAllowedOriginList errorRequests.notAllowedOrigin
        ]


noAllowedOriginHeaderList : List Har.Entry -> Html Msg
noAllowedOriginHeaderList =
    entryLinkList "Missing Access-Control-Allow-Origin Header" (ChangeView << NoAllowedHeader)


notAllowedOriginList : List Har.Entry -> Html Msg
notAllowedOriginList =
    entryLinkList "Request Origin Not Allowed" (ChangeView << NotAllowedOrigin)


entryLinkList : String -> (Har.Entry -> Msg) -> List Har.Entry -> Html Msg
entryLinkList title msg entries =
    if List.isEmpty entries then
        text ""

    else
        div []
            [ h2 [] [ text title ]
            , div [] (List.map (errorEntryView msg) entries)
            ]


errorEntryView : (Har.Entry -> Msg) -> Har.Entry -> Html Msg
errorEntryView msg entry =
    div []
        [ a [ onClick (msg entry) ] [ text entry.request.url ]
        ]


noAllowedHeaderView : Har.Entry -> Html Msg
noAllowedHeaderView entry =
    div [ style "margin-top" "1rem" ]
        [ a [ onClick (ChangeView Summary) ] [ text "<| Back" ]
        , h1 [] [ text "No Access-Control-Allow-Origin Header" ]
        , harEntryView
            { headerHighlights = Set.fromList [ "origin", "access-control-allow-origin" ] }
            entry
        ]


notAllowedOriginView : Har.Entry -> Html Msg
notAllowedOriginView entry =
    div [ style "margin-top" "1rem" ]
        [ a [ onClick (ChangeView Summary) ] [ text "<| Back" ]
        , h1 [] [ text "Origin Not Allowed" ]
        , harEntryView
            { headerHighlights = Set.fromList [ "origin", "access-control-allow-origin" ] }
            entry
        ]


harEntryView : { headerHighlights : Set String } -> Har.Entry -> Html Msg
harEntryView options entry =
    div []
        [ div [] [ text (Iso8601.fromTime entry.startedDateTime) ]
        , div [ class "cols-2" ]
            [ div [ class "hundred-col" ]
                [ requestView options entry ]
            , div [ class "hundred-col" ]
                [ responseView options entry ]
            ]
        ]


requestView : { headerHighlights : Set String } -> Har.Entry -> Html Msg
requestView { headerHighlights } { request } =
    div []
        [ h2 [] [ text "Request" ]
        , div [] [ text request.url ]
        , div []
            (h3 [] [ text "Headers" ]
                :: List.map (headerView headerHighlights) request.headers
            )
        ]


responseView : { headerHighlights : Set String } -> Har.Entry -> Html Msg
responseView { headerHighlights } { response } =
    div []
        [ h2 [] [ text "Response" ]
        , div [] [ text "Status: ", text (String.fromInt response.status), text " (", text response.statusText, text ")" ]
        , div []
            (h3 [] [ text "Headers" ]
                :: List.map (headerView headerHighlights) response.headers
            )
        ]


headerView : Set String -> Har.Header -> Html Msg
headerView highlights header =
    let
        highlightClass =
            if Set.member (String.toLower header.name) highlights then
                "highlight"

            else
                ""
    in
    div [ class highlightClass ]
        [ text header.name, text ": ", text header.value ]



-- Utilities


type alias CorsFailures =
    { noAllowedOriginHeader : List Har.Entry
    , notAllowedOrigin : List Har.Entry
    }


corsFailures : Har.Log -> CorsFailures
corsFailures log =
    log.entries
        |> List.foldl
            (\entry failures ->
                if entry.response.status < 200 then
                    failures

                else if isMissingAllowedOrigin entry then
                    { failures | noAllowedOriginHeader = entry :: failures.noAllowedOriginHeader }

                else if isNotAllowedOrigin entry then
                    { failures | notAllowedOrigin = entry :: failures.notAllowedOrigin }

                else
                    failures
            )
            { noAllowedOriginHeader = []
            , notAllowedOrigin = []
            }


isMissingAllowedOrigin : Har.Entry -> Bool
isMissingAllowedOrigin { request, response } =
    let
        originHeader =
            Har.header "origin" request

        allowHeader =
            Har.header "access-control-allow-origin" response
    in
    case ( originHeader, allowHeader ) of
        ( Just _, Nothing ) ->
            True

        _ ->
            False


isNotAllowedOrigin : Har.Entry -> Bool
isNotAllowedOrigin { request, response } =
    let
        originHeader =
            Har.header "origin" request

        allowHeader =
            Har.header "access-control-allow-origin" response
    in
    case ( originHeader, allowHeader ) of
        ( Just origin, Just allow ) ->
            not (String.contains origin.value allow.value || allow.value == "*")

        _ ->
            False
