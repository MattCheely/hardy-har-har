module Shared exposing (Model, Msg, harLog, init, requestFileView, update)

import File exposing (File)
import File.Select as Select
import Har
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (decodeString)
import Task



-- MODEL


type Model
    = NoData (Maybe LoadError)
    | Loaded Har.Log


type alias LoadError =
    { file : File
    , error : Decode.Error
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoData Nothing, Cmd.none )


harLog : Model -> Maybe Har.Log
harLog model =
    case model of
        NoData _ ->
            Nothing

        Loaded log ->
            Just log



-- UPDATE


type Msg
    = SelectFile
    | FileSelected File
    | FileStringified File String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectFile ->
            ( model, Select.file [ ".har", "application/json" ] FileSelected )

        FileSelected file ->
            ( model
            , Task.perform
                (FileStringified file)
                (File.toString file)
            )

        FileStringified file fileText ->
            case decodeString Har.decoder fileText of
                Ok log ->
                    ( Loaded log, Cmd.none )

                Err error ->
                    ( NoData (Just { file = file, error = error })
                    , Cmd.none
                    )


requestFileView : Model -> Html Msg
requestFileView model =
    let
        maybeError =
            case model of
                NoData err ->
                    err

                Loaded _ ->
                    Nothing
    in
    div []
        [ case maybeError of
            Just { error, file } ->
                text ("Sorry, I could not parse " ++ File.name file ++ " as a .har file")

            Nothing ->
                text ""
        , button [ onClick SelectFile ] [ text "Select a HAR file" ]
        ]
