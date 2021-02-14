module Main exposing (main)

import Browser exposing (Document)
import Duplicates
import Har
import Html exposing (..)
import Html.Events exposing (onClick)
import Redact
import Shared



-- MODEL


type alias Model =
    { shared : Shared.Model
    , viewing : View
    , duplicateState : Duplicates.Model
    , redactState : Redact.Model
    }


type View
    = Overview
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
    case Shared.harLog model.shared of
        Just log ->
            [ menuView
            , actionView log model
            ]

        Nothing ->
            [ loadFileView model ]


actionView : Har.Log -> Model -> Html Msg
actionView log model =
    case model.viewing of
        Overview ->
            text "Loaded"

        Duplicates ->
            Duplicates.view log model.duplicateState
                |> Html.map DuplicatesMsg

        Redact ->
            Redact.view log
                |> Html.map RedactMsg


menuView : Html Msg
menuView =
    div []
        [ button [ onClick (SelectView Duplicates) ] [ text "Find Duplicate Requests" ]
        , text " "
        , button [ onClick (SelectView Redact) ] [ text "Redact Data" ]
        , text " "
        , button [] [ text "Load A New File" ]
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
