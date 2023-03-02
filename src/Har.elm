module Har exposing
    ( Browser
    , CacheInfo
    , CacheUsage
    , Content
    , Cookie
    , Creator
    , Entry
    , Header
    , Log
    , Page
    , PageTiming
    , PostData
    , PostParam
    , QueryParam
    , Request
    , RequestTiming
    , Response
    , decoder
    , header
    , page
    )

import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import List.Extra as ListX
import Time exposing (Posix)


type alias Log =
    { version : String
    , creator : Creator
    , browser : Maybe Browser
    , pages : Dict String Page
    , entries : List Entry
    , comment : String
    }


type alias Creator =
    { name : String
    , version : String
    , comment : String
    }


type alias Browser =
    { name : String
    , version : String
    , comment : String
    }


type alias Page =
    { id : String
    , title : String
    , startedDateTime : Posix
    , pageTimings : PageTiming
    , comment : String
    }


type alias PageTiming =
    { onContentLoad : Maybe Float
    , onLoad : Maybe Float
    , comment : String
    }


type alias Entry =
    { pageref : Maybe String
    , startedDateTime : Posix
    , time : Float
    , request : Request
    , response : Response
    , cache : CacheUsage
    , timings : RequestTiming
    , serverIpAddress : Maybe String
    , connection : Maybe String
    , comment : String
    }


type alias Request =
    { method : String
    , url : String
    , httpVersion : String
    , cookies : List Cookie
    , headers : List Header
    , queryString : List QueryParam
    , postData : Maybe PostData
    , headersSize : Maybe Int
    , bodySize : Maybe Int
    , comment : String
    }


type alias Response =
    { status : Int
    , statusText : String
    , httpVersion : String
    , cookies : List Cookie
    , headers : List Header
    , content : Content
    , redirectUrl : String
    , headersSize : Maybe Int
    , bodySize : Maybe Int
    , comment : String
    }


type alias Cookie =
    { name : String
    , value : String
    , path : Maybe String
    , domain : Maybe String
    , expires : Maybe Posix
    , httpOnly : Bool
    , secure : Bool
    , comment : String
    }


type alias Header =
    { name : String
    , value : String
    , comment : String
    }


type alias QueryParam =
    { name : String
    , value : String
    , comment : String
    }


type alias PostData =
    { mimeType : String
    , params : List PostParam
    , text : String
    , comment : String
    }


type alias PostParam =
    { name : String
    , value : Maybe String
    , fileName : Maybe String
    , contentType : Maybe String
    , comment : String
    }


type alias Content =
    { size : Int
    , compression : Maybe Int
    , mimeType : String
    , text : Maybe String
    , encoding : Maybe String
    , comment : String
    }


type alias CacheUsage =
    { beforeRequest : Maybe CacheInfo
    , afterRequest : Maybe CacheInfo
    , comment : String
    }


type alias CacheInfo =
    { expires : Maybe Posix
    , lastAccess : Posix
    , etag : String
    , hitCount : Int
    , comment : String
    }


type alias RequestTiming =
    { blocked : Maybe Float
    , dns : Maybe Float
    , connect : Maybe Float
    , send : Float
    , wait : Float
    , receive : Float
    , ssl : Maybe Float
    , comment : String
    }



-- HELPERS


header : String -> { a | headers : List Header } -> Maybe Header
header headerName call =
    call.headers
        |> ListX.find (\h -> String.toLower h.name == String.toLower headerName)


page : Log -> Entry -> Maybe Page
page log entry =
    entry.pageref
        |> Maybe.andThen (\id -> Dict.get id log.pages)



-- DECODER


decoder : Decoder Log
decoder =
    (Decode.succeed Log
        |> required "version" Decode.string
        |> required "creator" decodeCreator
        |> maybe "browser" decodeBrowser
        |> required "pages" (decodeListDict .id decodePage)
        |> required "entries" (Decode.list decodeEntry)
        |> optional "comment" Decode.string ""
    )
        |> Decode.field "log"


decodeCreator : Decoder Creator
decodeCreator =
    Decode.map3 Creator
        (Decode.field "name" Decode.string)
        (Decode.field "version" Decode.string)
        decodeComment


decodeBrowser : Decoder Browser
decodeBrowser =
    Decode.map3 Browser
        (Decode.field "name" Decode.string)
        (Decode.field "version" Decode.string)
        decodeComment


decodePage : Decoder Page
decodePage =
    Decode.map5 Page
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "startedDateTime" decodeTimestamp)
        (Decode.field "pageTimings" decodePageTiming)
        decodeComment


decodePageTiming : Decoder PageTiming
decodePageTiming =
    Decode.map3 PageTiming
        (Decode.field "onContentLoad" decodeHarFloat)
        (Decode.field "onLoad" decodeHarFloat)
        decodeComment


decodeEntry : Decoder Entry
decodeEntry =
    Decode.succeed Entry
        |> maybe "pageref" Decode.string
        |> required "startedDateTime" decodeTimestamp
        |> required "time" Decode.float
        |> required "request" decodeRequest
        |> required "response" decodeResponse
        |> required "cache" decodeCacheUsage
        |> required "timings" decodeRequestTiming
        |> maybe "serverIpAddress" Decode.string
        |> maybe "connection" Decode.string
        |> optional "comment" Decode.string ""


decodeRequest : Decoder Request
decodeRequest =
    Decode.succeed Request
        |> required "method" Decode.string
        |> required "url" Decode.string
        |> required "httpVersion" Decode.string
        |> required "cookies" (Decode.list decodeCookie)
        |> required "headers" (Decode.list decodeHeader)
        |> required "queryString" (Decode.list decodeQueryParam)
        |> maybe "postData" decodePostData
        |> optional "headersSize" decodeHarInt Nothing
        |> optional "bodySize" decodeHarInt Nothing
        |> optional "comment" Decode.string ""


decodeResponse : Decoder Response
decodeResponse =
    Decode.succeed Response
        |> required "status" Decode.int
        |> required "statusText" Decode.string
        |> required "httpVersion" Decode.string
        |> required "cookies" (Decode.list decodeCookie)
        |> required "headers" (Decode.list decodeHeader)
        |> required "content" decodeContent
        |> required "redirectURL" Decode.string
        |> optional "headersSize" decodeHarInt Nothing
        |> optional "bodySize" decodeHarInt Nothing
        |> optional "comment" Decode.string ""


decodeCookie : Decoder Cookie
decodeCookie =
    Decode.succeed Cookie
        |> required "name" Decode.string
        |> required "value" Decode.string
        |> maybe "path" Decode.string
        |> maybe "domain" Decode.string
        |> maybe "expires" decodeTimestamp
        |> optional "httpOnly" Decode.bool False
        |> optional "secure" Decode.bool False
        |> optional "comment" Decode.string ""


decodeHeader : Decoder Header
decodeHeader =
    Decode.succeed Header
        |> required "name" Decode.string
        |> required "value" Decode.string
        |> optional "comment" Decode.string ""


decodeQueryParam : Decoder QueryParam
decodeQueryParam =
    Decode.succeed QueryParam
        |> required "name" Decode.string
        |> required "value" Decode.string
        |> optional "comment" Decode.string ""


{-| The spec says params is required, but chrome does not always provide it
-}
decodePostData : Decoder PostData
decodePostData =
    Decode.succeed PostData
        |> required "mimeType" Decode.string
        |> optional "params" (Decode.list decodePostParam) []
        |> required "text" Decode.string
        |> optional "comment" Decode.string ""


decodePostParam : Decoder PostParam
decodePostParam =
    Decode.succeed PostParam
        |> required "name" Decode.string
        |> maybe "value" Decode.string
        |> maybe "fileName" Decode.string
        |> maybe "contentType" Decode.string
        |> optional "comment" Decode.string ""


{-| The spec says size and mimeType are required, but Firefox will not include
them for blocked requests
-}
decodeContent : Decoder Content
decodeContent =
    Decode.succeed Content
        |> optional "size" Decode.int 0
        |> maybe "compression" Decode.int
        |> optional "mimeType" Decode.string ""
        |> maybe "text" Decode.string
        |> maybe "encoding" Decode.string
        |> optional "comment" Decode.string ""


decodeCacheUsage : Decoder CacheUsage
decodeCacheUsage =
    Decode.succeed CacheUsage
        |> maybe "beforeRequest" decodeCacheInfo
        |> maybe "afterRequest" decodeCacheInfo
        |> optional "comment" Decode.string ""


decodeCacheInfo : Decoder CacheInfo
decodeCacheInfo =
    Decode.succeed CacheInfo
        |> maybe "expires" decodeTimestamp
        |> required "lastAccess" decodeTimestamp
        |> required "etag" Decode.string
        |> required "hitCount" Decode.int
        |> optional "comment" Decode.string ""


{-| The spec says send, wait, and receive are not optional, but FF may not
include them for blocked requests
-}
decodeRequestTiming : Decoder RequestTiming
decodeRequestTiming =
    Decode.succeed RequestTiming
        |> optional "blocked" decodeHarFloat Nothing
        |> optional "dns" decodeHarFloat Nothing
        |> optional "connect" decodeHarFloat Nothing
        |> optional "send" Decode.float 0
        |> optional "wait" Decode.float 0
        |> optional "receive" Decode.float 0
        |> optional "ssl" decodeHarFloat Nothing
        |> optional "comment" Decode.string ""


{-| The HAR format does an odd thing where it uses -1 to represent an
inapplicable span of milliseconds or # of bytes. We just decode that to Nothing
-}
decodeHarInt : Decoder (Maybe Int)
decodeHarInt =
    Decode.int
        |> Decode.andThen
            (\int ->
                if int >= 0 then
                    Decode.succeed (Just int)

                else
                    Decode.succeed Nothing
            )


decodeHarFloat : Decoder (Maybe Float)
decodeHarFloat =
    Decode.float
        |> Decode.andThen
            (\float ->
                if float >= 0 then
                    Decode.succeed (Just float)

                else
                    Decode.succeed Nothing
            )


decodeTimestamp : Decoder Posix
decodeTimestamp =
    Decode.string
        |> Decode.andThen
            (\string ->
                case Iso8601.toTime string of
                    Ok posix ->
                        Decode.succeed posix

                    Err _ ->
                        Decode.fail (string ++ " is not a valid ISO8601 timestamp")
            )


{-| Comment decoding always falls back to an empty string
-}
decodeComment : Decoder String
decodeComment =
    Decode.oneOf
        [ Decode.field "comment" Decode.string
        , Decode.succeed ""
        ]


decodeListDict : (a -> comparable) -> Decoder a -> Decoder (Dict comparable a)
decodeListDict extractKey decodeItem =
    Decode.list
        (decodeItem
            |> Decode.map (\item -> ( extractKey item, item ))
        )
        |> Decode.map Dict.fromList


maybe field fieldDecoder =
    optional field (Decode.nullable fieldDecoder) Nothing
