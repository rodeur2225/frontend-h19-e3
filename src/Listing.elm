module Listing exposing (Model, decoder)

import Json.Decode exposing (Decoder, field, list, map, string)



-- MODEL


type alias Model =
    { title : String }



-- INIT


init : String -> ( Model, Cmd Msg )
init title =
    ( Model title, Cmd.none )



-- UPDATE


type Msg
    = Nothing



-- DECODER


titleDecoder : Decoder String
titleDecoder =
    field "title" string


listingDecoder : Decoder Model
listingDecoder =
    map Model titleDecoder


decoder : Decoder (List Model)
decoder =
    field "listings" (list listingDecoder)
