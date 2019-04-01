module Page.BookListing exposing (Model, Msg(..), init, update, view)

import Api
import Api.Endpoint exposing (bookListing, listing)
import Css exposing (..)
import Form.AvailabilityInput
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (checked, css, for, hidden, id, type_, value)
import Html.Styled.Events exposing (onCheck, onSubmit)
import Http
import Iso8601
import Json.Encode as Encode
import Listing
import Session
import Skeleton
import Time
import Ui



-- MODEL


type alias Model =
    { session : Session.Data
    , listing : Status Listing.Model
    , bookings : List Booking
    }


type Status a
    = Failure
    | Loading
    | Success a


type alias Booking =
    Time.Posix


init : Session.Data -> String -> ( Model, Cmd Msg )
init session id =
    ( Model session Loading [], Api.get (listing id) GotListing Listing.listingDecoder )



-- UPDATE


type Msg
    = GotListing (Result Http.Error Listing.Model)
    | AddBooking Booking
    | RemoveBooking Booking
    | SubmitBookings
    | ListingBooked (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotListing result ->
            case result of
                Err _ ->
                    ( { model | listing = Failure }, Cmd.none )

                Ok listings ->
                    ( { model | listing = Success listings }, Cmd.none )

        AddBooking booking ->
            ( { model | bookings = booking :: model.bookings }, Cmd.none )

        RemoveBooking booking ->
            ( { model | bookings = List.filter (\b -> b /= booking) model.bookings }, Cmd.none )

        SubmitBookings ->
            case model.listing of
                Success listing ->
                    ( model
                    , Api.post (bookListing listing.id) (encodeBookings model.bookings) ListingBooked
                    )

                _ ->
                    ( model, Cmd.none )

        ListingBooked _ ->
            ( model, Cmd.none )


encodeBookings : List Booking -> Encode.Value
encodeBookings bookings =
    Encode.object
        [ ( "bookings", Encode.list Iso8601.encode bookings ) ]



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    case model.listing of
        Failure ->
            { title = "Listing", content = [ text "Could not find listing..." ] }

        Loading ->
            { title = "Listing", content = [ text "Loading listing..." ] }

        Success listing ->
            { title = listing.title
            , content =
                [ div [ css [ displayFlex, flexDirection row ] ]
                    [ viewListing listing
                    , viewAvailabilities model.bookings listing.availabilities
                    ]
                ]
            }


viewListing : Listing.Model -> Html Msg
viewListing listing =
    div [ css [ padding (px 16), width (pct 50) ] ]
        [ h1
            [ css
                [ fontWeight (int 300)
                , marginBottom (px 4)
                ]
            ]
            [ text listing.title ]
        , h2
            [ css
                [ fontWeight (int 300)
                , marginBottom (px 12)
                , fontSize (rem 1)
                , color (rgba 0 0 0 0.6)
                ]
            ]
            [ text listing.owner.name ]
        , p [] [ text listing.description ]
        , p [] [ text listing.price, text "$" ]
        ]


viewAvailabilities : List Booking -> List Booking -> Html Msg
viewAvailabilities bookings availabilities =
    let
        viewAvailabilityPartial =
            Form.AvailabilityInput.view bookings AddBooking RemoveBooking
    in
    div [ css [ padding (px 16), width (pct 50) ] ]
        [ h1
            [ css
                [ fontWeight (int 300)
                , marginBottom (px 4)
                ]
            ]
            [ text "Disponibilités" ]
        , form
            [ css
                [ displayFlex
                , flexDirection column
                ]
            , onSubmit SubmitBookings
            ]
            [ div [ css [ displayFlex, flexDirection row ] ] (List.map viewAvailabilityPartial availabilities)
            , div [] [ Ui.submit [ value "Réserver" ] [] ]
            ]
        ]
