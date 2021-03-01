module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Dict
import Duplicates
import Har
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Redact
import Shared exposing (HarData)
import Timeline



-- MODEL


type alias Model =
    { shared : Shared.Model
    , viewing : View
    , duplicateState : Duplicates.Model
    , redactState : Redact.Model
    , timelineState : Timeline.Model
    , windowSize : ( Int, Int )
    }


type View
    = Overview
    | LoadFile
    | Duplicates
    | Redact


type alias Flags =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { shared = Shared.init
      , viewing = Overview
      , duplicateState = Duplicates.init
      , redactState = Redact.init
      , timelineState = Timeline.init
      , windowSize = ( flags.width, flags.height )
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SelectView View
    | SharedMsg Shared.Msg
    | DuplicatesMsg Duplicates.Msg
    | RedactMsg Redact.Msg
    | SizeChange Int Int
    | TimelineMsg Timeline.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SizeChange width height ->
            ( { model | windowSize = ( width, height ) }, Cmd.none )

        SharedMsg sharedMsg ->
            let
                ( sharedModel, sharedCmd ) =
                    Shared.update sharedMsg model.shared
            in
            ( { model | shared = sharedModel }, Cmd.map SharedMsg sharedCmd )

        SelectView newView ->
            ( { model | viewing = newView }, Cmd.none )

        DuplicatesMsg duplicatesMsg ->
            ( { model | duplicateState = Duplicates.update duplicatesMsg model.duplicateState }
            , Cmd.none
            )

        RedactMsg redactMsg ->
            ( { model | redactState = Redact.update redactMsg model.redactState }, Cmd.none )

        TimelineMsg timelineMsg ->
            ( { model | timelineState = Timeline.update timelineMsg model.timelineState }, Cmd.none )



-- VIEW


body : Model -> List (Html Msg)
body model =
    case Shared.data model.shared of
        Just harData ->
            [ menuView
            , actionView harData model
            ]

        Nothing ->
            [ loadFileView model ]


actionView : HarData -> Model -> Html Msg
actionView { name, log } model =
    case model.viewing of
        LoadFile ->
            loadFileView model

        Overview ->
            div []
                [ summaryView name log
                , Timeline.view log model.windowSize model.timelineState
                    |> Html.map TimelineMsg
                ]

        Duplicates ->
            Duplicates.view log model.duplicateState
                |> Html.map DuplicatesMsg

        Redact ->
            Redact.view log
                |> Html.map RedactMsg


menuView : Html Msg
menuView =
    div []
        [ text " | "
        , a [ onClick (SelectView Overview), class "tertiary" ] [ text "Summary" ]
        , text " | "
        , a [ onClick (SelectView Duplicates), class "tertiary" ] [ text "Duplicate Requests" ]
        , text " | "
        , a [ onClick (SelectView Redact), class "tertiary" ] [ text "Redact Data" ]
        , text " | "
        , button [ onClick (SelectView LoadFile), class "tertiary" ] [ text "Load A Different File" ]
        ]


summaryView : String -> Har.Log -> Html Msg
summaryView fileName log =
    let
        pageCount =
            Dict.size log.pages

        requestCount =
            List.length log.entries

        pageText =
            if pageCount == 1 then
                " on a single page"

            else
                " spanning " ++ String.fromInt pageCount ++ " pages"

        requestText =
            if requestCount == 1 then
                "request"

            else
                "requests"
    in
    div []
        [ h2 [] [ text fileName ]
        , text (String.fromInt requestCount)
        , text " "
        , text requestText
        , text pageText
        ]


loadFileView : Model -> Html Msg
loadFileView model =
    Html.map SharedMsg (Shared.requestFileView model.shared)


view : Model -> Document Msg
view model =
    { title = "Hardy Har Har"
    , body = body model
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize SizeChange



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
