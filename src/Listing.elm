module Listing exposing (Model, decoder)

import Json.Decode exposing (Decoder, field, list, map, map2, string)



-- MODEL


type alias Model =
    { id : String, title : String }



-- INIT


init : String -> String -> ( Model, Cmd Msg )
init id title =
    ( Model id title, Cmd.none )



-- UPDATE


type Msg
    = Nothing



-- DECODER


idDecoder : Decoder String
idDecoder =
    field "id" string


titleDecoder : Decoder String
titleDecoder =
    field "title" string


listingDecoder : Decoder Model
listingDecoder =
    map2 Model idDecoder titleDecoder


decoder : Decoder (List Model)
decoder =
    field "listings" (list listingDecoder)
