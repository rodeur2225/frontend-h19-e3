module Page.BookListing exposing (Model, Msg(..), init, update, view)

import Css exposing (..)
import Html.Styled exposing (Html, div, h1, h2, p, text)
import Html.Styled.Attributes exposing (css)
import Http
import Listing
import Session
import Skeleton



-- MODEL


type alias Model =
    { session : Session.Data
    , listing : Status Listing.Model
    }


type Status a
    = Failure
    | Loading
    | Success a


init : Session.Data -> String -> ( Model, Cmd Msg )
init session id =
    ( Model session Loading
    , Http.get
        { url = "/listings/" ++ id ++ ".json"
        , expect = Http.expectJson GotListing Listing.listingDecoder
        }
    )



-- UPDATE


type Msg
    = GotListing (Result Http.Error Listing.Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotListing result ->
            case result of
                Err _ ->
                    ( { model | listing = Failure }, Cmd.none )

                Ok listings ->
                    ( { model | listing = Success listings }, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    case model.listing of
        Failure ->
            { title = "Listing", content = [ text "Could not find listing..." ] }

        Loading ->
            { title = "Listing", content = [ text "Loading listing..." ] }

        Success listing ->
            { title = listing.title, content = [ viewListing listing ] }


viewListing : Listing.Model -> Html Msg
viewListing listing =
    div [ css [ padding (px 16) ] ]
        [ h1 [ css [ fontWeight (int 300), marginBottom (px 4) ] ] [ text listing.title ]
        , h2 [ css [ fontWeight (int 300), marginBottom (px 12), fontSize (rem 1), color (rgba 0 0 0 0.6) ] ] [ text listing.owner ]
        , p [] [ text listing.description ]
        ]
