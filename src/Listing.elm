module Listing exposing (Model, decoder, listingDecoder)

import Json.Decode exposing (Decoder, field, list, map4, string)



-- MODEL


type alias Model =
    { id : String, title : String, owner : String, description : String }



-- INIT


init : String -> String -> String -> String -> ( Model, Cmd Msg )
init id title owner description =
    ( Model id title owner description, Cmd.none )



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


ownerDecoder : Decoder String
ownerDecoder =
    field "owner" string


descriptionDecoder : Decoder String
descriptionDecoder =
    field "description" string


listingDecoder : Decoder Model
listingDecoder =
    map4 Model idDecoder titleDecoder ownerDecoder descriptionDecoder


decoder : Decoder (List Model)
decoder =
    field "listings" (list listingDecoder)
