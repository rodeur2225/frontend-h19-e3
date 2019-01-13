module Page.Listings exposing (Model, Msg, init, update, view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
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
        { url = "/listings.json"
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
    { title = "Listings"
    , content =
        [ div [ css [ padding (px 16) ] ]
            [ h1 [ css [ fontWeight (int 300) ] ] [ text "Services et échanges disponibles" ]
            , viewListings model.listings
            ]
        ]
    }


viewListings : Status (List Listing.Model) -> Html Msg
viewListings listings =
    case listings of
        Failure ->
            text "Could not load listings!"

        Loading ->
            text "Loading listings..."

        Success l ->
            div [ css [ displayFlex ] ] (List.map viewListing l)


viewListing : Listing.Model -> Html Msg
viewListing listing =
    a
        [ css
            [ display block
            , borderRadius (px 20)
            , border3 (px 1) solid (rgba 0 0 0 0.05)
            , margin (px 8)
            , padding2 (px 8) (px 12)
            , fontSize (rem 1)
            , textDecoration none
            , color (hex "000")
            , hover
                [ boxShadow4 (px 0) (px 1) (px 3) (rgba 0 0 0 0.5) ]
            ]
        , href ("/listings/" ++ listing.id)
        ]
        [ text listing.title ]
