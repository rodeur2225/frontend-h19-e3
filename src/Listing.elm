module Listing exposing (Model, decoder, empty, encode, listingDecoder)

import Iso8601
import Json.Decode exposing (Decoder, field, list, map, map3, map5, map7, string, int)
import Json.Decode.Extra exposing (datetime)
import Json.Encode as Encode
import Time



-- MODEL


type alias Model =
    { id : String
    , title : String
    , owner : Owner
    , description : String
    , availabilities : List Time.Posix
    , price : String
    , feedbacks : List String
    }


type alias Owner =
    { name : String, phoneNumber : String, email : String }


empty : Model
empty =
    Model "" "" emptyOwner "" [] "" []


emptyOwner : Owner
emptyOwner =
    Owner "" "" ""



-- INIT


init : String -> String -> Owner -> String -> List Time.Posix -> String -> List String -> ( Model, Cmd Msg )
init id title owner description availabilities price feedbacks =
    ( Model id title owner description availabilities price feedbacks, Cmd.none )



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


ownerDecoder : Decoder Owner
ownerDecoder =
    field "owner"
        (map3 Owner
            (field "name" string)
            (field "phoneNumber" string)
            (field "email" string)
        )


descriptionDecoder : Decoder String
descriptionDecoder =
    field "description" string


availabilitiesDecoder : Decoder (List Time.Posix)
availabilitiesDecoder =
    field "availabilities" (list datetime)

priceDecoder : Decoder String
priceDecoder=
    field "price" string

feedbacksDecoder : Decoder (List String)
feedbacksDecoder =
    field "feedbacks" (list string)


listingDecoder : Decoder Model
listingDecoder =
    map7 Model idDecoder titleDecoder ownerDecoder descriptionDecoder availabilitiesDecoder priceDecoder feedbacksDecoder


decoder : Decoder (List Model)
decoder =
    field "listings" (list listingDecoder)


encode : Model -> Encode.Value
encode listing =
    Encode.object
        [ ( "title", Encode.string listing.title )
        , ( "owner", encodeOwner listing.owner )
        , ( "description", Encode.string listing.description )
        , ( "price", Encode.string listing.price)
        ]


encodeOwner : Owner -> Encode.Value
encodeOwner owner =
    Encode.object
        [ ( "name", Encode.string owner.name )
        , ( "phoneNumber", Encode.string owner.phoneNumber )
        , ( "email", Encode.string owner.email )
        ]
