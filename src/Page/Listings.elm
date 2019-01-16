module Page.Listings exposing (Model, Msg, init, update, view)

import Api
import Api.Endpoint exposing (createListing, listings)
import Css exposing (..)
import Form.AvailabilityInput
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, for, href, id, name, placeholder, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import Listing
import Session
import Skeleton
import Time



-- MODEL


type alias Model =
    { session : Session.Data
    , listings : Status (List Listing.Model)
    , listing : Listing.Model
    }


type Status a
    = Failure
    | Loading
    | Success a


type alias Availability =
    Time.Posix



-- INIT


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( Model session Loading Listing.empty, Api.get listings GotListings Listing.decoder )



-- UPDATE


type Msg
    = GotListings (Result Http.Error (List Listing.Model))
    | AddAvailability Availability
    | RemoveAvailability Availability
    | ChangeTitle String
    | ChangeOwner String
    | ChangeDescription String
    | SubmitListing
    | ListingCreated (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotListings result ->
            case result of
                Err _ ->
                    ( { model | listings = Failure }, Cmd.none )

                Ok listings ->
                    ( { model | listings = Success listings }, Cmd.none )

        SubmitListing ->
            ( model, Api.post createListing (Listing.encode model.listing) ListingCreated )

        ListingCreated _ ->
            case model.listings of
                Success listings ->
                    ( { model | listings = Success (model.listing :: listings), listing = Listing.empty }, Cmd.none )

                _ ->
                    ( { model | listings = Success [ model.listing ], listing = Listing.empty }, Cmd.none )

        AddAvailability avail ->
            let
                listing =
                    model.listing
            in
            ( { model | listing = { listing | availabilities = avail :: listing.availabilities } }, Cmd.none )

        RemoveAvailability avail ->
            let
                listing =
                    model.listing
            in
            ( { model | listing = { listing | availabilities = List.filter (\a -> a /= avail) listing.availabilities } }, Cmd.none )

        ChangeTitle title ->
            let
                listing =
                    model.listing
            in
            ( { model | listing = { listing | title = title } }, Cmd.none )

        ChangeOwner owner ->
            let
                listing =
                    model.listing
            in
            ( { model | listing = { listing | owner = owner } }, Cmd.none )

        ChangeDescription description ->
            let
                listing =
                    model.listing
            in
            ( { model | listing = { listing | description = description } }, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    { title = "Listings"
    , content =
        [ div [ css [ displayFlex ] ]
            [ div
                [ css
                    [ width (pct 50)
                    , padding (px 16)
                    ]
                ]
                [ h1 [ css [ fontWeight (int 300) ] ] [ text "Services et échanges disponibles" ]
                , viewListings model.listings
                ]
            , div
                [ css
                    [ width (pct 50)
                    , padding (px 16)
                    ]
                ]
                [ h1 [ css [ fontWeight (int 300) ] ] [ text "Ajouter une offre" ]
                , viewListingForm model.listing
                ]
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


viewListingForm : Listing.Model -> Html Msg
viewListingForm listing =
    let
        viewAvailabilityPartial =
            Form.AvailabilityInput.view listing.availabilities AddAvailability RemoveAvailability
    in
    form
        [ onSubmit SubmitListing
        ]
        [ label [] [ text "Titre" ]
        , input [ onInput ChangeTitle, value listing.title, placeholder "Titre" ] []
        , label [] [ text "Propriétaire" ]
        , input [ onInput ChangeOwner, value listing.owner, placeholder "Propriétaire" ] []
        , label [] [ text "Description" ]
        , textarea [ onInput ChangeDescription ] [ text listing.description ]
        , label [] [ text "Disponibilités" ]
        , div [ css [ displayFlex, flexDirection row ] ] (List.map viewAvailabilityPartial generateAvailabilities)
        , input
            [ type_ "submit"
            , value "Ajouter"
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


generateAvailabilities : List Availability
generateAvailabilities =
    List.map (\i -> Time.millisToPosix (1547596800000 + (i * 86400000))) (List.range 1 7)
