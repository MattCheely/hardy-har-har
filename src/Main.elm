module Main exposing (main)

import Browser exposing (Document)
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
    }


type View
    = Overview
    | LoadFile
    | Duplicates
    | Redact


init : () -> ( Model, Cmd Msg )
init flags =
    let
        ( sharedModel, sharedCmd ) =
            Shared.init flags
    in
    ( { shared = sharedModel
      , viewing = Overview
      , duplicateState = Duplicates.init
      , redactState = Redact.init
      }
    , Cmd.map SharedMsg sharedCmd
    )



-- UPDATE


type Msg
    = SelectView View
    | SharedMsg Shared.Msg
    | DuplicatesMsg Duplicates.Msg
    | RedactMsg Redact.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                , Timeline.view log
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
subscriptions =
    always Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
