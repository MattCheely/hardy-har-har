module Redact exposing (Model, Msg, init, update, view)

import Collection exposing (addToGroup)
import Dict exposing (Dict)
import Har exposing (Cookie, Header, QueryParam)
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onCheck)
import Set exposing (Set)



-- MODEL


type alias Model =
    { selectedHeaders : Set String
    , selectedCookies : Set String
    , selectedQueryParams : Set String
    , selectedRiskyStrings : Set String
    }


init : Model
init =
    { selectedHeaders = Set.empty
    , selectedCookies = Set.empty
    , selectedQueryParams = Set.empty
    , selectedRiskyStrings = Set.empty
    }


type TextLocation
    = Header String
    | Cookie String
    | Query String



-- UPDATE


type Msg
    = SelectHeader String Bool
    | SelectCookie String Bool
    | SelectQueryParam String Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectHeader header selected ->
            { model
                | selectedHeaders =
                    if selected then
                        Set.insert header model.selectedHeaders

                    else
                        Set.remove header model.selectedHeaders
            }

        SelectCookie cookie selected ->
            { model
                | selectedCookies =
                    if selected then
                        Set.insert cookie model.selectedCookies

                    else
                        Set.remove cookie model.selectedCookies
            }

        SelectQueryParam param selected ->
            { model
                | selectedQueryParams =
                    if selected then
                        Set.insert param model.selectedQueryParams

                    else
                        Set.remove param model.selectedQueryParams
            }



-- VIEW


view : Har.Log -> Html Msg
view log =
    let
        riskyHeaders =
            collectRiskyHeaders log

        riskyCookies =
            collectRiskyCookies log

        riskyQueryParams =
            collectRiskyQueryParams log
    in
    div []
        (if
            Dict.isEmpty riskyHeaders
                && Dict.isEmpty riskyCookies
                && Dict.isEmpty riskyQueryParams
         then
            [ text "No Risky Data found" ]

         else
            [ if not (Dict.isEmpty riskyHeaders) then
                riskyHeadersView riskyHeaders

              else
                text ""
            , if not (Dict.isEmpty riskyCookies) then
                riskyCookiesView riskyCookies

              else
                text ""
            , if not (Dict.isEmpty riskyQueryParams) then
                riskyQueryParamsView riskyQueryParams

              else
                text ""
            ]
        )


riskyHeadersView : Dict String (List Header) -> Html Msg
riskyHeadersView riskyHeaders =
    riskyItemsView "Headers" SelectHeader riskyHeaders


riskyCookiesView : Dict String (List Cookie) -> Html Msg
riskyCookiesView riskyCookies =
    riskyItemsView "Cookies" SelectCookie riskyCookies


riskyQueryParamsView : Dict String (List QueryParam) -> Html Msg
riskyQueryParamsView riskyQueryParams =
    riskyItemsView "QueryParams" SelectQueryParam riskyQueryParams


riskyItemsView : String -> (String -> Bool -> Msg) -> Dict String (List a) -> Html Msg
riskyItemsView itemName selectAction riskyItems =
    div []
        [ h2 [] [ text "Potentially Risky ", text itemName ]
        , div []
            (Dict.toList riskyItems
                |> List.map (riskyItemView selectAction)
            )
        ]


riskyItemView : (String -> Bool -> Msg) -> ( String, List a ) -> Html Msg
riskyItemView selectMsg ( name, occurences ) =
    div []
        [ label []
            [ input [ type_ "checkbox", onCheck (selectMsg name) ] []
            , span [] []
            , text " "
            , text name
            ]
        ]


collectRiskyHeaders : Har.Log -> Dict String (List Header)
collectRiskyHeaders log =
    List.foldl
        (\entry collected ->
            collected
                |> addRiskyHeaders entry.request.headers
        )
        Dict.empty
        log.entries


addRiskyHeaders : List Header -> Dict String (List Header) -> Dict String (List Header)
addRiskyHeaders headers collection =
    List.foldl
        (\header collected ->
            if isRiskyHeader header then
                addToGroup .name header collected

            else
                collected
        )
        collection
        headers


isRiskyHeader : Header -> Bool
isRiskyHeader header =
    (header.name == "Authorization")
        || (header.name == "Proxy-Authorization")
        || String.startsWith "X-" header.name
        || String.startsWith "x-" header.name


collectRiskyCookies : Har.Log -> Dict String (List Cookie)
collectRiskyCookies log =
    List.foldl
        (\entry collected ->
            collected
                |> addRiskyCookies entry.request.cookies
        )
        Dict.empty
        log.entries


addRiskyCookies : List Cookie -> Dict String (List Cookie) -> Dict String (List Cookie)
addRiskyCookies cookies collection =
    List.foldl
        (\cookie collected ->
            if isRiskyCookie cookie then
                addToGroup .name cookie collected

            else
                collected
        )
        collection
        cookies


isRiskyCookie : Cookie -> Bool
isRiskyCookie cookie =
    True


collectRiskyQueryParams : Har.Log -> Dict String (List QueryParam)
collectRiskyQueryParams log =
    List.foldl
        (\entry collected ->
            collected
                |> addRiskyQueryParams entry.request.queryString
        )
        Dict.empty
        log.entries


addRiskyQueryParams : List QueryParam -> Dict String (List QueryParam) -> Dict String (List QueryParam)
addRiskyQueryParams queryParams collection =
    List.foldl
        (\queryParam collected ->
            if isRiskyQueryParam queryParam then
                addToGroup .name queryParam collected

            else
                collected
        )
        collection
        queryParams


isRiskyQueryParam : QueryParam -> Bool
isRiskyQueryParam queryParam =
    True
