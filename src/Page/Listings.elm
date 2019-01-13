module Page.Listings exposing (Model, Msg, init, update, view)

import Html.Styled exposing (Html, div, h1, text)
import Http
import Json.Decode as Decode
import Listing
import Session
import Skeleton



-- MODEL


type alias Model =
    { session : Session.Data
    , listings : Status (List Listing.Model)
    }


type Status a
    = Failure
    | Loading
    | Success a



-- INIT


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( Model session Loading
    , Http.get
        { url = "/listings"
        , expect = Http.expectJson GotListings Listing.decoder
        }
    )



-- UPDATE


type Msg
    = GotListings (Result Http.Error (List Listing.Model))


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotListings result ->
            case result of
                Err _ ->
                    ( { model | listings = Failure }, Cmd.none )

                Ok listings ->
                    ( { model | listings = Success listings }, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    { title = "Listings", content = [ viewListings model.listings ] }


viewListings : Status (List Listing.Model) -> Html Msg
viewListings listings =
    case listings of
        Failure ->
            text "Could not load listings!"

        Loading ->
            text "Loading listings..."

        Success l ->
            div [] (List.map viewListing l)


viewListing : Listing.Model -> Html Msg
viewListing listing =
    h1 [] [ text listing.title ]
