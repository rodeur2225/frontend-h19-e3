module Page.BookListing exposing (Model, Msg(..), init, update, view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (checked, css, for, hidden, id, type_, value)
import Html.Styled.Events exposing (onCheck, onSubmit)
import Http
import Iso8601
import Json.Encode as Encoder
import Listing
import Session
import Skeleton
import Time



-- MODEL


type alias Model =
    { session : Session.Data
    , listing : Status Listing.Model
    , bookings : List Time.Posix
    }


type Status a
    = Failure
    | Loading
    | Success a


init : Session.Data -> String -> ( Model, Cmd Msg )
init session id =
    ( Model session Loading []
    , Http.get
        { url = "/listings/" ++ id ++ ".json"
        , expect = Http.expectJson GotListing Listing.listingDecoder
        }
    )



-- UPDATE


type Msg
    = GotListing (Result Http.Error Listing.Model)
    | AddBooking Time.Posix
    | RemoveBooking Time.Posix
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
                    , Http.post
                        { url = "/listings/" ++ listing.id ++ "/book"
                        , body = Http.jsonBody (encodeBookings model.bookings)
                        , expect = Http.expectWhatever ListingBooked
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        ListingBooked _ ->
            ( model, Cmd.none )


encodeBookings : List Time.Posix -> Encoder.Value
encodeBookings bookings =
    Encoder.object
        [ ( "bookings", Encoder.list Iso8601.encode bookings ) ]



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
            [ text listing.owner ]
        , p [] [ text listing.description ]
        ]


viewAvailabilities : List Time.Posix -> List Time.Posix -> Html Msg
viewAvailabilities bookings availabilities =
    let
        viewAvailabilityPartial =
            viewAvailability bookings
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
            , div []
                [ input
                    [ type_ "submit"
                    , value "Réserver"
                    , css
                        [ backgroundColor transparent
                        , margin (px 8)
                        , padding2 (px 8) (px 12)
                        , borderRadius (px 20)
                        , border (px 0)
                        , cursor pointer
                        , fontWeight bold
                        , fontSize (rem 1)
                        , textTransform uppercase
                        , hover
                            [ boxShadow4 (px 0) (px 1) (px 3) (rgba 0 0 0 0.5) ]
                        ]
                    ]
                    []
                ]
            ]
        ]


viewAvailability : List Time.Posix -> Time.Posix -> Html Msg
viewAvailability bookings availability =
    let
        inputId =
            String.fromInt (Time.posixToMillis availability)

        day =
            String.fromInt (Time.toDay Time.utc availability)

        humanDay =
            toFrenchWeekday (Time.toWeekday Time.utc availability)

        booked =
            List.member availability bookings
    in
    div
        []
        [ input
            [ id inputId
            , type_ "checkbox"
            , hidden True
            , onCheck
                (\checked ->
                    if checked then
                        AddBooking availability

                    else
                        RemoveBooking availability
                )
            ]
            []
        , label
            [ for inputId
            , css
                [ displayFlex
                , flexDirection column
                , alignItems center
                , borderRadius (px 20)
                , border3 (px 1) solid (rgba 0 0 0 0.05)
                , margin (px 8)
                , padding2 (px 4) (px 8)
                , fontSize (rem 1)
                , textDecoration none
                , color (hex "000")
                , bookedStyle booked
                , cursor pointer
                , hover
                    [ boxShadow4 (px 0) (px 1) (px 3) (rgba 0 0 0 0.5) ]
                ]
            ]
            [ span
                [ css
                    [ padding2 (px 8) (px 12)
                    , borderBottom3 (px 1) solid (rgba 0 0 0 0.05)
                    , fontSize (rem 1.2)
                    ]
                ]
                [ text day ]
            , span
                [ css
                    [ padding2 (px 8) (px 12)
                    , fontSize (rem 1)
                    ]
                ]
                [ text humanDay ]
            ]
        ]


bookedStyle : Bool -> Style
bookedStyle booked =
    if booked then
        backgroundColor (rgba 0 0 0 0.1)

    else
        backgroundColor transparent


toFrenchWeekday : Time.Weekday -> String
toFrenchWeekday weekday =
    case weekday of
        Time.Mon ->
            "Lun"

        Time.Tue ->
            "Mar"

        Time.Wed ->
            "Mer"

        Time.Thu ->
            "Jeu"

        Time.Fri ->
            "Ven"

        Time.Sat ->
            "Sam"

        Time.Sun ->
            "Dim"
