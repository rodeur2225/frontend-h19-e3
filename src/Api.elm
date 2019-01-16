module Api exposing (get, post)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value)
import Url exposing (Url)



-- HTTP


get : Endpoint -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
get url toMsg decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers = []
        , body = Http.emptyBody
        }


post : Endpoint -> Decode.Value -> (Result Http.Error () -> msg) -> Cmd msg
post url body toMsg =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectWhatever toMsg
        , headers = []
        , body = Http.jsonBody body
        }
