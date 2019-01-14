module Listing exposing (Model, decoder, listingDecoder)

import Json.Decode exposing (Decoder, field, list, map, map5, string)
import Json.Decode.Extra exposing (datetime)
import Time



-- MODEL


type alias Model =
    { id : String, title : String, owner : String, description : String, availabilities : List Time.Posix }



-- INIT


init : String -> String -> String -> String -> List Time.Posix -> ( Model, Cmd Msg )
init id title owner description availabilities =
    ( Model id title owner description availabilities, Cmd.none )



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


availabilitiesDecoder : Decoder (List Time.Posix)
availabilitiesDecoder =
    field "availabilities" (list datetime)


listingDecoder : Decoder Model
listingDecoder =
    map5 Model idDecoder titleDecoder ownerDecoder descriptionDecoder availabilitiesDecoder


decoder : Decoder (List Model)
decoder =
    field "listings" (list listingDecoder)
